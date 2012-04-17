package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJProgressBar;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class VabraSolver {
	private ArrayList<Integer> downSampleFactor; //list of resolutions at which to register the images
	private ArrayList<Integer> resolutionSwitchPoints; //list of levels at which we go to the next downSampleFactor

	private int currentLevelIdx; // track current level
	private int currentResolutionIdx; //track where we are in the multi-resolution scheme

	private ArrayList<Integer> gridSpacingX, gridSpacingY, gridSpacingZ; //list of uniform gridpoints at all the levels
	float interval_x, interval_y, interval_z; //spacing of uniform grid points at current level
	int gridPoints; //number of uniform grid points
	int[][] grid; //list of uniform grid points (x,y,z coordinates) at current level  
	int xPts,yPts,zPts; //convenience variables used in conjunction w/ uniform grid

	int[][] reverseIdxGrid;
	byte[][][] gridVisited;

	//boolean fineOptimize;
	boolean saveIntermResults;
	//boolean useMNMI;

	VabraSubjectTargetPairs imgSubTarPairs; //subject/target pair of images
	VabraOptimizer optimizer;
	File outputDir;


	double[][] gradients;
	float[] gradMag;
	int[] index;

	public void dispose(){
		optimizer.dispose();
		imgSubTarPairs.dispose();
	}

	//	configFile is an XML file containing program options
	//	st contains subject/target pair
	public VabraSolver(VabraSubjectTargetPairs imgSubTarPairs,File configFile, File outputDir, 
			boolean saveIntermResults, double[] directionsOptmizationWeight,int defFieldUpdateMode)
	{
		this.outputDir = outputDir;
		this.saveIntermResults = saveIntermResults;
		//if(defFieldUpdateMode == 0) this.fineOptimize = fineOptimize;
		//else this.fineOptimize = false; //at the moment fine optimize only works with the original update type
		this.imgSubTarPairs=imgSubTarPairs;
		//if(useMNMI) optimizer = new VabraOptimizerMNMI(imgSubTarPairs, parent, directionsToOptmize, defFieldUpdateMode); 
		//else 
		optimizer = new VabraOptimizer(imgSubTarPairs, directionsOptmizationWeight, defFieldUpdateMode); 
		grid=null;
		readConfigFile(configFile);
	}

	//	configFile is an XML based file with program options
	void readConfigFile(File f)
	{
		VabraConfiguration configVabra=new VabraConfiguration(f);
		downSampleFactor=configVabra.getDownSampleResolutions();
		resolutionSwitchPoints=configVabra.getResolutionSwitches();
		resolutionSwitchPoints.add(0, 0);
		gridSpacingX=configVabra.getLevels();
		gridSpacingY=configVabra.getLevels();
		gridSpacingZ=configVabra.getLevels();
		if(downSampleFactor.size()!=resolutionSwitchPoints.size())
		{
			System.out.format("The number of switch points(%d) should be one less than the number of resolutions(%d)\n",(int)resolutionSwitchPoints.size()-1,(int)downSampleFactor.size());
		}
	}

	public void registerImages()
	{
        final ViewJProgressBar progressBar = new ViewJProgressBar("vabra",
                "vabra deformation field...", 0, 100, false, null, null);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(0);
		
		currentResolutionIdx=0;
		//setTotalUnits(gridSpacingX.size()*2+1);
		for(currentLevelIdx=0;currentLevelIdx<gridSpacingX.size();currentLevelIdx++){
			//1.) Change Resolution if Necessary
			if(currentResolutionIdx <resolutionSwitchPoints.size()){				
				if(resolutionSwitchPoints.get(currentResolutionIdx)==currentLevelIdx)
				{
					//System.out.format("Changing image resolution %d\n",downSampleFactor.get(currentResolutionIdx));
					imgSubTarPairs.setResolution(downSampleFactor.get(currentResolutionIdx));

					//System.out.format("After Set Resolution%f/%f\n", (float)Runtime.getRuntime().freeMemory(), (float)Runtime.getRuntime().totalMemory());
					currentResolutionIdx++;
				}
			}

			//2.) Prepare data and histograms for level
			imgSubTarPairs.prepareForNextLevel();

			//3.) Register at level
			saveIntermediateResults();
			registerAtCurrentLevel();
			progressBar.updateValueImmed( (int)(100 * currentLevelIdx / (float)gridSpacingX.size()) );
		}
		saveIntermediateResults();
		//4.)Return to original resolution
		if(downSampleFactor.get(currentResolutionIdx-1)!=1) imgSubTarPairs.setResolution(1.0f);
		imgSubTarPairs.prepareForNextLevel();
        progressBar.updateValueImmed(100);
        progressBar.dispose();
	}

	void saveIntermediateResults(){
		if((outputDir != null) && saveIntermResults){
			File intermOutputDir = new File(outputDir.toString()+File.separator+"VABRAIntermResults");
			intermOutputDir.mkdir();

			int oldXN = imgSubTarPairs.subject.getXN(); 
			int oldYN = imgSubTarPairs.subject.getYN(); 
			int oldZN = imgSubTarPairs.subject.getZN(); 

			int newXN = imgSubTarPairs.origSubjectList.get(0).getExtents().length > 0 ? imgSubTarPairs.origSubjectList.get(0).getExtents()[0] : 1;
			int newYN = imgSubTarPairs.origSubjectList.get(0).getExtents().length > 1 ? imgSubTarPairs.origSubjectList.get(0).getExtents()[1] : 1;
			int newZN = imgSubTarPairs.origSubjectList.get(0).getExtents().length > 2 ? imgSubTarPairs.origSubjectList.get(0).getExtents()[2] : 1;

			ModelImage defField = new ModelImage( ModelStorageBase.FLOAT, new int[]{newXN, newYN, newZN, 3}, "temp" );

			//Resample Deformation Field back to original resolution and save
			RegistrationUtilities.DeformationFieldResample3DM(imgSubTarPairs.getDeformationField(), defField, oldXN, oldYN, oldZN, newXN, newYN, newZN);
			defField.setImageName(imgSubTarPairs.origSubjectList.get(0).getImageName()+"_def_field_lvl"+currentLevelIdx);
			ModelImage.saveImage( defField, defField.getImageName(), intermOutputDir+File.separator );			

			defField.disposeLocal();
		}
	}

	void registerAtCurrentLevel()
	{
		// 1. generate a uniform grid based on parameters from config file
		generateGrid(gridSpacingX.get(currentLevelIdx),gridSpacingY.get(currentLevelIdx),gridSpacingZ.get(currentLevelIdx));

		// 2. Identify regions of mismatch based on local gradient wrt NMI
		System.gc();
		identifyRegions();

		// 3. Maximize NMI over the regions identified
		//System.out.println(getClass().getCanonicalName()+"\t"+"OPTIMIZE AT GRID POINTS");
		optimizeAtGridPoints();

		// 4. Fine Optimize if necessary
		//if(fineOptimize) fineOptimizeAtGridPoints();
	}

	//	identify regions of high gradient
	void identifyRegions()
	{

		int i,gradParam;
		gradParam=imgSubTarPairs.coarseGradientParameters();
		gradients=new double[gridPoints][gradParam];
		gradMag=new float[gridPoints];

		//Set the scale of local deformations based on the current grid spacing
		optimizer.getRBF().setScale(interval_x, interval_y, interval_z);

		//find the gradient wrt a CostFunction at each grid point 
		for(i=0;i<gridPoints;i++)
		{
			//System.out.format("COARSE GRADIENT %d/%d\n",i,gridPoints);
			optimizer.coarseGradient(grid[i],gradients[i]);

			gradMag[i]=(float)RegistrationUtilities.VectorNormalization(gradients[i],gradParam);

		}
	}

	void optimizeAtGridPoints(){

		int idx;
		int i,j;
		//Sort in order of gradient magnitude
		index=RegistrationUtilities.QKSort2(gradMag);
		// Note that since gradMag is sorted, index allows us to refer back to the original
		// grid points

		//optimize at gridpoints, starting with the point with highest gradient magnitude

		for(j=0;j<gridPoints;j++)
		{
			i=gridPoints-1-j;
			idx=index[i];
			//System.out.format("At ("+grid[idx][0]+","+grid[idx][1]+","+grid[idx][2]+") Mag:"+gradMag[i]+"\n");
			//If gradMag meets threshold, then optimize
			if (gradMag[i]>.00001) {
				optimizer.coarseOptimize(grid[idx],gradients[idx]);
			}
		}
	}
	
	void generateGrid(int xPoints, int yPoints, int zPoints)
	{
		xPts=xPoints;
		yPts=yPoints;
		zPts=zPoints;
		generateGrid();
	}

	void generateGrid()
	{

		int box[]=new int[6];
		box=imgSubTarPairs.getBoundingBox();

		if(grid!=null)
		{
			grid=null;
			gridVisited=null;
			reverseIdxGrid=null;
			index=null;
		}
		grid = new int[xPts*yPts*zPts][3];
		reverseIdxGrid = new int[xPts*yPts*zPts][3];
		gridVisited = new byte[xPts][yPts][zPts];

		interval_x = (float)(box[1] - box[0])/(xPts-1);
		interval_y = (float)(box[3] - box[2])/(yPts-1);
		interval_z = (float)(box[5] - box[4])/(zPts-1);

		int count = 0;
		for (int i=0; i<xPts; i++) for (int j=0; j<yPts; j++) for (int k=0; k<zPts; k++) {
			grid[count][0] = (int)Math.floor(box[0] + i*interval_x+0.5);
			grid[count][1] = (int)Math.floor(box[2] + j*interval_y+0.5);
			grid[count][2] = (int)Math.floor(box[4] + k*interval_z+0.5);

			reverseIdxGrid[count][0]=i;
			reverseIdxGrid[count][1]=j;
			reverseIdxGrid[count][2]=k;

			gridVisited[i][j][k]=0;
			count++;
		}

		gridPoints=zPts*yPts*xPts;
		index = new int[gridPoints];
	}

	void generateFinePoints(int[] point,int[][] newPoints)
	{	
		newPoints[0][0]=point[0]-(int)(interval_x/2.0);
		newPoints[0][1]=point[1]-(int)(interval_y/2.0);
		newPoints[0][2]=point[2]+(int)(interval_z/2.0);

		newPoints[1][0]=point[0]-(int)(interval_x/2.0);
		newPoints[1][1]=point[1]+(int)(interval_y/2.0);
		newPoints[1][2]=point[2]-(int)(interval_z/2.0);

		newPoints[2][0]=point[0]-(int)(interval_x/2.0);
		newPoints[2][1]=point[1]+(int)(interval_y/2.0);
		newPoints[2][2]=point[2]+(int)(interval_z/2.0);

		newPoints[3][0]=point[0]+(int)(interval_x/2.0);
		newPoints[3][1]=point[1]-(int)(interval_y/2.0);
		newPoints[3][2]=point[2]-(int)(interval_z/2.0);

		newPoints[4][0]=point[0]+(int)(interval_x/2.0);
		newPoints[4][1]=point[1]-(int)(interval_y/2.0);
		newPoints[4][2]=point[2]+(int)(interval_z/2.0);

		newPoints[5][0]=point[0]+(int)(interval_x/2.0);
		newPoints[5][1]=point[1]+(int)(interval_y/2.0);
		newPoints[5][2]=point[2]-(int)(interval_z/2.0);

		newPoints[6][0]=point[0]+(int)(interval_x/2.0);
		newPoints[6][1]=point[1]+(int)(interval_y/2.0);
		newPoints[6][2]=point[2]+(int)(interval_z/2.0);

		newPoints[7][0]=point[0]-(int)(interval_x/2.0);
		newPoints[7][1]=point[1]-(int)(interval_y/2.0);
		newPoints[7][2]=point[2]-(int)(interval_z/2.0);
	}

	boolean beenVisited(int gridIdx)
	{
		int px,py,pz;
		px=reverseIdxGrid[gridIdx][0];
		py=reverseIdxGrid[gridIdx][1];
		pz=reverseIdxGrid[gridIdx][2];
		if(gridVisited[px][py][pz]==0) return false;
		else return true;	
	}

	void markVisited(int gridIdx)
	{
		int px,py,pz,x,y,z;
		px=reverseIdxGrid[gridIdx][0];
		py=reverseIdxGrid[gridIdx][1];
		pz=reverseIdxGrid[gridIdx][2];
		//System.out.format("(%d %d %d) -. (%d %d %d)\n",grid[gridIdx][0],grid[gridIdx][1],grid[gridIdx][2],px,py,pz);

		gridVisited[px][py][pz]=1;

		//000
		x=Math.max(0,px-1);
		y=Math.max(0,py-1);
		z=Math.max(0,pz-1);
		gridVisited[x][y][z]=1;

		//001
		x=Math.max(0,px-1);
		y=Math.max(0,py-1);
		z=Math.min(zPts-1,pz+1);
		gridVisited[x][y][z]=1;

		//010
		x=Math.max(0,px-1);
		y=Math.min(yPts-1,py+1);
		z=Math.max(0,pz-1);
		gridVisited[x][y][z]=1;

		//011
		x=Math.max(0,px-1);
		y=Math.min(yPts-1,py+1);
		z=Math.min(zPts-1,pz+1);
		gridVisited[x][y][z]=1;

		//100
		x=Math.min(xPts-1,px+1);
		y=Math.max(0,py-1);
		z=Math.max(0,pz-1);
		gridVisited[x][y][z]=1;

		//101
		x=Math.min(xPts-1,px+1);
		y=Math.max(0,py-1);
		z=Math.min(zPts-1,pz+1);
		gridVisited[x][y][z]=1;

		//110
		x=Math.min(xPts-1,px+1);
		y=Math.min(yPts-1,py+1);
		z=Math.max(0,pz-1);
		gridVisited[x][y][z]=1;

		//111
		x=Math.min(xPts-1,px+1);
		y=Math.min(yPts-1,py+1);
		z=Math.min(zPts-1,pz+1);
		gridVisited[x][y][z]=1;

	}

	public List<ModelImage> getDeformedSubject(){
		return imgSubTarPairs.getDeformedSubject();
	}

	public ModelImage getDeformationField(){
		return imgSubTarPairs.getDeformationField();
	}
}
