package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import Jama.Matrix;

public class DTIEPIDistortionCorrection extends AlgorithmBase {
    // The ImageData ordering, such as in vol3d = new float[rows][columns][slices] in ImageDataFloat, 
	// is opposite the storage order actually used in ModelImage in which the slices change least frequently
	// and the rows change most frequently as you increment thru the data.  The ModelImage ordering would correspond
	// to float[slices][columns][rows]
	
	//Internal Variables
	int XN, YN, ZN; 
	float[] dimRes;
	// Assume this means stored in color
	int chN = 3;
	ModelImage destImage[];
	List<File> DWDirectionToB0TransformationsFileList;
	// Rigid B0 to Structural Transformation Matrix
	Matrix rigidB0toStruct;
	
	// Before calling create destImage[] array with:
	// int numImages = DWDirectionToB0TransformationsFileList.size();
	//ModelImage destImage[] = new ModelImage[numImages];
	// for (i = 0; i < numImages; i++) {
	    // String fileName = DWDirectionToB0TransformationsFileList.get(i).getName();
	    // int index = fileName.indexOf('.');
	    // String newName = fileName.substring(0,index) + "_combDefField";
	    // destImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), newName);
    // } // for (i = 0; i < numImages; i++)
	
	public DTIEPIDistortionCorrection(ModelImage destImage[], ModelImage srcImage, List<File> DWDirectionToB0TransformationsFileList,
			Matrix rigidB0toStruct) {
		super(null, srcImage);
		this.destImage = destImage;
		this.DWDirectionToB0TransformationsFileList = DWDirectionToB0TransformationsFileList;
		this.rigidB0toStruct = rigidB0toStruct;
	}
	
	public void runAlgorithm() {
		//this.setLabel("combine Volumes");
		ModelImage EPIDistortionCorrectionFieldImage = srcImage;
		XN = EPIDistortionCorrectionFieldImage.getExtents()[0];
		YN = EPIDistortionCorrectionFieldImage.getExtents()[1];
		ZN = EPIDistortionCorrectionFieldImage.getExtents()[2];
		dimRes = EPIDistortionCorrectionFieldImage.getResolutions(0);
		// Use 4 rather than chN = 3 because we store ARGB
		int sliceSize = XN * YN;
		int length = 4 * sliceSize * ZN;
		float EPICorrectDef[] = new float[length];
		try {
			EPIDistortionCorrectionFieldImage.exportData(0, length, EPICorrectDef);
		}
		catch (IOException error) {
			errorCleanUp("IOException on EPICorrectionDefImage.exportData: " + error, true);
			return;
		}
		
		float currentDef[][][][];
		float[][][][] EPICorrectDefSplit = new float[chN][XN][YN][ZN];
		for (int c = 0; c < chN; c++) {
		    for (int x = 0; x < XN; x++) {
		    	for (int y = 0; y < YN; y++) {
		    		for (int z = 0; z < ZN; z++) {
		    			EPICorrectDefSplit[c][x][y][z] = EPICorrectDef[c + 1 + 4*(x + y*XN + z*sliceSize)];
		    		}
		    	}
		    }
		}
		EPICorrectDef = null;
		
		ArrayList<double[][]> arrayDWtoB0Trans = readMultiMatrices(DWDirectionToB0TransformationsFileList);
		Matrix m;
		double[][] matarray;
		
		for (int i = 0; i < arrayDWtoB0Trans.size(); i++){
			//File currentFile = DWtoB0Trans.get(i);
			//apply transformations or deformations one by one
			//currentDef = new ImageDataFloat[chN];
			 
			
				m = new Matrix(4,4);
				matarray = arrayDWtoB0Trans.get(i);
				if(matarray.length!=4 || matarray[0].length!=4){
					System.err.println(getClass().getCanonicalName()+"Invalid transformation - must be 4x4");
				}else{
					for(int k=0; k<matarray.length; k++){
						for(int j=0; j<matarray[0].length; j++){
							m.set(k, j, matarray[k][j]);
						}
					}
				}
			
				m = rigidB0toStruct.times(m);
				System.out.format("hi\n");
				for(int k=0;k<4;k++)for(int l=0;l<4;l++)System.out.format(m.get(k,l)+"\n");
				
			currentDef=createDefFieldFromTransMatrix(m);
			currentDef=applyDeformation(currentDef, EPICorrectDefSplit);
			
			float outputBuffer[] = new float[length];
			for (int c = 0; c < chN; c++) {
			    for (int x = 0; x < XN; x++) {
			    	for (int y = 0; y < YN; y++) {
			    		for (int z = 0; z < ZN; z++) {
			    			outputBuffer[c + 1 + 4*(x + y*XN + z*sliceSize)] = currentDef[c][x][y][z];
			    		}
			    	}
			    }
			}
			currentDef = null;
			
			try {
                destImage[i].importData(0, outputBuffer, true);
            } catch (IOException error) {
                errorCleanUp("IOException on destImage[" + i + "].importData: " + error, true);

                return;
            }

            JDialogBase.updateFileInfo(srcImage,destImage[i]);
			
		} // for (int i = 0; i < arrayDWtoB0Trans.size(); i++)
		
		setCompleted(true);
		return;
		
	}
	
	private ArrayList<double[][]> readMultiMatrices(List<File> files){
		ArrayList<double[][]> allxfms = new ArrayList<double[][]>(files.size());
		int i=0;
		while(i<files.size()){
			String fileName = files.get(i).getName();
			if (fileName == null) {
				return null;
			}
			if (!files.get(i).exists()) {
				return null;
			}
			try {
			allxfms.add(readObject(files.get(i)));
			}
			catch(Exception e) {
				e.printStackTrace();
				return null;
			}
			i++;
		}
		return allxfms;
	}
	
	protected double[][] readObject(File f) {
		BufferedReader in;
		// ArrayList<ArrayList<Double>> datArray=new
		// ArrayList<ArrayList<Double>>();
		// ArrayList<Double> array;
		try {
			// Create input stream from file
			in = new BufferedReader(new InputStreamReader(
					new FileInputStream(f)));
			String str;
			// Read file as string
			int rows = Integer.parseInt(in.readLine());
			int cols = Integer.parseInt(in.readLine());
			double[][] dat = new double[rows][cols];
			int i = 0;
			while ((str = in.readLine()) != null && i<rows) {
				// array=new ArrayList<Double>();
				String[] strs = str.split(" ");
				int j = 0;
				for (String s : strs) {
					try {
						// array.add(Double.parseDouble(s));
						dat[i][j] = Double.parseDouble(s);
					} catch (NumberFormatException e) {
					}
					j++;
				}
				// datArray.add(array);
				i++;
			}
			in.close();
			// double[][] dat=new double[datArray.size()][0];
			// for(int i=0;i<datArray.size();i++){
			// array=datArray.get(i);
			// dat[i]=new double[array.size()];
			// for(int j=0;j<array.size();j++){
			// dat[i][j]=array.get(j);
			// }
			// }
			return dat;
		} catch (Exception e) {
			System.err.println(getClass().getCanonicalName()+"Error occured while reading parameter file:\n"
					+ e.getMessage());
			e.printStackTrace();
			return null;
		}
	}
	
	public float[][][][] applyDeformation(float[][][][] currentDef, float[][][][] epiDef){
		double[] currentVec = new double[chN];
		double[] newVec = new double[chN];
		
		float[][][][] newDef = new float[chN][XN][YN][ZN];
		for (int c = 0; c < chN; c++) {
			for(int i = 0; i < XN; i++) {
				for(int j = 0; j < YN; j++) {
					for(int k = 0; k < ZN; k++){
					   newDef[c][i][j][k] = currentDef[c][i][j][k];	
					}
				}
			}
		}
		

		for(int i = 0; i < XN; i++)
			for(int j = 0; j < YN; j++)
				for(int k = 0; k < ZN; k++){
					
					for(int c = 0; c < chN; c++) newVec[c] = (double)epiDef[c][i][j][k]; 
					
					//get current deformation at where the new deformation is pointing
					for(int c = 0; c < chN; c++) currentVec[c] = TrilinearInterpolation(currentDef[c], XN, YN, ZN, 
							newVec[0]+i, newVec[1]+j, newVec[2]+k); 
					
					for(int c = 0; c < chN; c++) newDef[c][i][j][k] = (float)(newVec[c]+currentVec[c]);
					
				}
		return newDef;			
		
	}
	
	public double TrilinearInterpolation(float[][][] oldV, int XN, int YN,
			int ZN, double x, double y, double z) {
		int i0, j0, k0, i1, j1, k1;
		double dx, dy, dz, hx, hy, hz;
		if (x < 0 || x > (XN - 1) || y < 0 || y > (YN - 1) || z < 0
				|| z > (ZN - 1)) {
			return 0;
		} else {
			j1 = (int) Math.ceil(x);
			i1 = (int) Math.ceil(y);
			k1 = (int) Math.ceil(z);
			j0 = (int) Math.floor(x);
			i0 = (int) Math.floor(y);
			k0 = (int) Math.floor(z);
			dx = x - j0;
			dy = y - i0;
			dz = z - k0;

			// Introduce more variables to reduce computation
			hx = 1.0 - dx;
			hy = 1.0 - dy;
			hz = 1.0 - dz;
			// Optimized below
			return   (((oldV[j0][i0][k0] * hx + oldV[j1][i0][k0] * dx) * hy 
					 + (oldV[j0][i1][k0] * hx + oldV[j1][i1][k0] * dx) * dy) * hz 
					+ ((oldV[j0][i0][k1] * hx + oldV[j1][i0][k1] * dx) * hy 
					 + (oldV[j0][i1][k1] * hx + oldV[j1][i1][k1] * dx) * dy)* dz);

		}
	}
	
	public float[][][][] createDefFieldFromTransMatrix(Matrix newTrans){
		// newTrans is 4 x 4
		// double[] currentVec = new double[chN];
		double[] newVec = new double[chN];
		float[][][][] newDef = new float[chN][XN][YN][ZN];
		
		for(int i = 0; i < XN; i++)
			for(int j = 0; j < YN; j++)
				for(int k = 0; k < ZN; k++){
					
					newVec[0] = i*dimRes[0];
					newVec[1] = j*dimRes[1];
					newVec[2] = k*dimRes[2];
					                	
                    Matrix v = new Matrix(new double[]{newVec[0],newVec[1],newVec[2],1},4);
                    /** Construct a matrix from a one-dimensional packed array
                    @param vals One-dimensional array of doubles, packed by columns (ala Fortran).
                    @param m    Number of rows.
                    @exception  IllegalArgumentException Array length must be a multiple of m.
                    */

                    /*public Matrix (double vals[], int m) {
                       this.m = m;
                       n = (m != 0 ? vals.length/m : 0);
                       if (m*n != vals.length) {
                          throw new IllegalArgumentException("Array length must be a multiple of m.");
                       }
                       A = new double[m][n];
                       for (int i = 0; i < m; i++) {
                          for (int j = 0; j < n; j++) {
                             A[i][j] = vals[i+j*m];
                          }
                       }
                    }*/
                    /*
                     * m = 4;
                     * n = 1;
                     * v = new double[4][1];
                     * v[0][0] = newVec[0];
                     * v[1][0] = newVec[1];
                     * v[2][0] = newVec[2];
                     * v[3][0] = 1;
                     */
                    Matrix vp = newTrans.solve(v);
                    /** Solve A*X = B
                    @param B    right hand side
                    @return     solution if A is square, least squares solution otherwise
                    */
                    for(int c = 0; c < chN; c++) {
                        newVec[c] = vp.get(c,0) - newVec[c]; 
                    }
                    
					//get current deformation at where the new deformation is pointing
					//for(int c = 0; c < chN; c++) currentVec[c] = RegistrationUtilities.Interpolation(currentDef[c], XN, YN, ZN, 
					//		newVec[0]+i, newVec[1]+j, newVec[2]+k, InterpolationType.TRILINEAR); 

					//for(int c = 0; c < chN; c++) newDef[c].set(i,j,k, newVec[c]+currentVec[c]);
					
					for(int c = 0; c < chN; c++) newDef[c][i][j][k] = (float)( newVec[c]/dimRes[c]);
					
				}
		
		return newDef;
		
	}
}