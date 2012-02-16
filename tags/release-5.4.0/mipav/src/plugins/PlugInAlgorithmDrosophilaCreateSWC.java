import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;

import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.util.MipavMath;


public class PlugInAlgorithmDrosophilaCreateSWC extends AlgorithmBase {

	private ModelImage finalImage, greenImage;

	private File filamentFile;
	
	private float greenThreshold, subsamplingDistance;
	
	private JTextArea outputTextArea;
	
    private ArrayList <ArrayList<float[]>> allFilamentCoords_swc = new ArrayList <ArrayList<float[]>>();
    
    private ArrayList <ArrayList<float[]>> newFilamentCoords_swc = new ArrayList<ArrayList<float[]>>();
	
    private float[] resols;


	public PlugInAlgorithmDrosophilaCreateSWC(ModelImage finalImage, File filamentFile, float greenThreshold, float subsamplingDistance, JTextArea outputTextArea) {
		this.finalImage = finalImage;
		this.resols = finalImage.getResolutions(0);
		this.filamentFile = filamentFile;
		this.greenThreshold = greenThreshold;
		this.subsamplingDistance = subsamplingDistance;
		this.outputTextArea = outputTextArea;
		
	}
	
	
	public void runAlgorithm() {
		outputTextArea.append("Running Algorithm v1.0" + "\n");
        
   

        final long begTime = System.currentTimeMillis();
        
        
		createSWCFile();
		
		final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;
        
        if(greenImage != null) {
        	greenImage.disposeLocal();
        	greenImage = null;
        }
        
        if(finalImage != null) {
        	finalImage.disposeLocal();
        	finalImage = null;
        }

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

	}
	
	
	
	
	
	
	
	
	
	
	
	
	 /**
     * creates the swc file
     * 
     * www.neuronland.org/NLMorphologyConverter/MorphologyFormats/SWC/Spec.html
     * 
     */
    private void createSWCFile() {
    	outputTextArea.append("Creating SWC file..." + "\n");
        outputTextArea.append("\n");

    	readFilamentFile_swc(filamentFile);
    	determineConnectivity1_swc(allFilamentCoords_swc);
    	determineAxon_swc(allFilamentCoords_swc);
		determineDistances_swc(allFilamentCoords_swc);
		outputTextArea.append("SWC - subsampling..." + "\n");
        outputTextArea.append("\n");
		subsample_swc(allFilamentCoords_swc,newFilamentCoords_swc);
		outputTextArea.append("SWC - determining connectivity..." + "\n");
        outputTextArea.append("\n");
		determineConnectivity2_swc(newFilamentCoords_swc);
		
		createGreenImage();
		
		/*outputTextArea.append("SWC - determining radii via region grow/distance map..." + "\n");
        outputTextArea.append("\n");
        determineRadiiRegionGrow_swc(newFilamentCoords_swc);
        
        output_swc(outputFilename_regionGrow,newFilamentCoords_swc);
		outputTextArea.append("Saving region grow SWC file to " + filamentFileParentDir + File.separator + outputFilename_auto + "\n");
        outputTextArea.append("\n");
        
        
		outputTextArea.append("SWC - determining radii autolatically..." + "\n");
        outputTextArea.append("\n");
		determineRadiiAutomatically_swc(newFilamentCoords_swc);
		
		output_swc(outputFilename_auto,newFilamentCoords_swc);
		outputTextArea.append("Saving automatic SWC file to " + filamentFileParentDir + File.separator + outputFilename_auto + "\n");
        outputTextArea.append("\n");*/

		outputTextArea.append("SWC - determining radd via threshold..." + "\n");
        outputTextArea.append("\n");
		determineRadiiThreshold_swc(newFilamentCoords_swc);
		
		String outputFilename = filamentFile.getName().substring(0, filamentFile.getName().indexOf(".")) + ".swc";
		output_swc(outputFilename,newFilamentCoords_swc);
		outputTextArea.append("Saving threshold SWC file to " + filamentFile.getParent() + File.separator + outputFilename + "\n");
        outputTextArea.append("\n");
        
        
		
		//new ViewJFrameImage(maskImageAuto);
		//new ViewJFrameImage(maskImage);
        //new ViewJFrameImage(maskImageRegionGrow);
    }
    
    
    
    
    
    /**
	 * creates green channel image
	 */
	private void createGreenImage() {
		int extents[] = finalImage.getExtents();
		int length = finalImage.getExtents()[0] * finalImage.getExtents()[1] * finalImage.getExtents()[2];
		float[] greenBuffer = new float[length];
		try {
			finalImage.exportRGBData(2, 0, length, greenBuffer);
		}catch (Exception e) {
			e.printStackTrace();
		}
		/////////////////////////////////////////////////////////////////////////////////////////////////////////
		//We need to pass the green channel through a median filter or Coherence-Enhancing Diffusion
		greenImage = new ModelImage(ModelStorageBase.FLOAT, extents, "greenImage");
		try {
            greenImage.importData(0, greenBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }
		FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[finalImage.getExtents()[2]];
		float[] finalImageResols = new float[3];
        finalImageResols[0] = finalImage.getResolutions(0)[0];
        finalImageResols[1] = finalImage.getResolutions(0)[1];
        finalImageResols[2] = finalImage.getResolutions(0)[2];
        /*float[] f = null;
        if (r7_27Coord_transformed != null) {
            f = new float[3];
            f[0] = -r7_27Coord_transformed[0];
            f[1] = -r7_27Coord_transformed[1];
            f[2] = -r7_27Coord_transformed[2];
        }*/
		for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(greenImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setUnitsOfMeasure(finalImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImageResols);
            fileInfoBases[i].setExtents(finalImage.getExtents());
           // if (r7_27Coord_transformed != null) {

                //fileInfoBases[i].setOrigin(f);
            //} else {
                fileInfoBases[i].setOrigin(finalImage.getFileInfo()[0].getOrigin());
            //}

            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);

        }
		greenImage.setFileInfo(fileInfoBases);
        greenImage.calcMinMax();
        //new ViewJFrameImage(greenImage);
	}
    
    
    
    
    
    /**
	 * reads filament file
	 * @param filamaneFile
	 * @return
	 */
	private boolean readFilamentFile_swc(File surfaceFile) {
		boolean success = true;
		RandomAccessFile raFile = null;
		try {

			raFile = new RandomAccessFile(surfaceFile, "r");
			
			String line;
			
			
			while((line=raFile.readLine())!= null) {
				line = line.trim();
				if(line.startsWith("Translate1Dragger")) {
					break;
				}
				if(line.contains("Coordinate3")) {
					ArrayList<float[]> filamentCoords = new ArrayList<float[]>();
					while(!((line=raFile.readLine()).endsWith("}"))) {
						line = line.trim();
						if(!line.equals("")) {
							if(line.startsWith("point [")) {
								line = line.substring(line.indexOf("point [") + 7, line.length()).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith("]")) {
								line = line.substring(0, line.indexOf("]")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith(",")) {
								line = line.substring(0, line.indexOf(",")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							String[] splits = line.split("\\s+");
							splits[0] = splits[0].trim();
							splits[1] = splits[1].trim();
							splits[2] = splits[2].trim();
							float coord_x = new Float(splits[0]).floatValue();
							float coord_y = new Float(splits[1]).floatValue();
							float coord_z = new Float(splits[2]).floatValue();
							float x = coord_x;
							float y = coord_y;
							float z = coord_z;
							//x,y,z are the coordinates
							//next one is distance from beginning of segment to the point///for now initialize at 0
							//next one is connectivity...initialize at 0
							//next one is axon...2=axon   3=dendrite    initialize at 3
							float[] coords = {x,y,z,0,0,3};
							
							filamentCoords.add(coords);
						}
					}
					allFilamentCoords_swc.add(filamentCoords);
				}
				
				
				
				
				
			}
			raFile.close();
			
		}catch(Exception e) {
			try {
				if(raFile != null) {
					raFile.close();
				}
			}catch(Exception ex) {
				
			}
			e.printStackTrace();
			return false;
		}
		
		return success;
	}
    
    
    
    
    
    
    
    
    
	 /**
     * Determines initial conenctivity...how one block is connected to the previous blocks
	 * while we are at it...lets set the type value to axon (2) for the 0th block
	 * @return
	 */
	private void determineConnectivity1_swc(ArrayList <ArrayList<float[]>> filamentCoords) {
		//first block of code stays at -1
		try {
			int allFilamentsSize = filamentCoords.size();
			int alMatchSize;
			ArrayList<float[]> al;
			ArrayList<float[]> al2;
			ArrayList<float[]> alMatch;
			float[] coords = new float[6];
			float[] coords2;
			float[] coordsMatch = new float[6];
			al = filamentCoords.get(0);

			for(int m=0;m<al.size();m++) {
				coords = al.get(m);
				if(m==0) {
					coords[4] = -1;
				}
			    coords[5] = 2;
			    al.set(m, coords);
				
			}
			
			//HACK
			//I am using the allFilamnetCoords to determine connectivity b/c for some reason some
			// of the transformed filament points are not exact
			//
			//MAY BE DELETING THE ABOVE COMMENT SOON   5/13/2011
			
			
			
			/*for(int i=1;i<allFilamentsSize;i++) {
				 al = filamentCoords.get(i);
				 coords = al.get(0);
				 al2 = allFilamentCoords_newCoords.get(i);
				 coords2 = al2.get(0);

				 int k;
				 
				 for(k=0;k<i;k++) {
					 alMatch = allFilamentCoords_newCoords.get(k);
					 alMatchSize = alMatch.size();
					 coordsMatch[0] = alMatch.get(alMatchSize-1)[0];
					 coordsMatch[1] = alMatch.get(alMatchSize-1)[1];
					 coordsMatch[2] = alMatch.get(alMatchSize-1)[2];
					 if(coords2[0]==coordsMatch[0] && coords2[1]==coordsMatch[1] && coords2[2]==coordsMatch[2]) {
						 //set the connectivity of coords[4] to k+1
						 coords[4] = k+1;
						 al.set(0, coords);

						 break;
					 }
					 
				 }
				 
		   }*/
			
			
			
			
			
			for(int i=1;i<allFilamentsSize;i++) {
				
				
				 al = filamentCoords.get(i);
				 coords = al.get(0);
				
				int k;
				 
				 for(k=0;k<i;k++) {
					 alMatch = filamentCoords.get(k);
					 alMatchSize = alMatch.size();
					 
					 coordsMatch[0] = alMatch.get(alMatchSize-1)[0];
					 coordsMatch[1] = alMatch.get(alMatchSize-1)[1];
					 coordsMatch[2] = alMatch.get(alMatchSize-1)[2];
					 
					 if(coords[0]==coordsMatch[0] && coords[1]==coordsMatch[1] && coords[2]==coordsMatch[2]) {

						 coords[4] = k+1;
						 al.set(0, coords);


						 break;
					 }
					 
					 
					 
				 }
				
				
			}
			
			
			
			
			
			
			
			
			
			
			//make sure all are connected
			for(int i=1;i<allFilamentsSize;i++) {
				 al = filamentCoords.get(i);
				 coords = al.get(0);
				 if(coords[4] == 0) {
					 //this means this block is not connected!
					 String coordsString = coords[0] + "," + coords[1] + "," + coords[2];
					 System.out.println("Standardized IV file is not properly connecte: the block of points starting with " + coordsString + " is not connected to anything");
					 
					 break;
					 
				 }
				 
			}
			
			

		}catch(Exception e) {
			e.printStackTrace();
		}

	}
    
	
	
	
	  /**
     * Dtermines is block of points is axon or dendritic brach
     */
    private void determineAxon_swc(ArrayList <ArrayList<float[]>> filamentCoords) {
		//first find block of points that has highest z-value
		int allFilamentsSize = filamentCoords.size();
		ArrayList<float[]> al;
		int alSize;
		float[] coords = new float[6];
		float zVal = 0;
		float z;
		int highestZBlockIndex = 0;
		float connectedTo = 0;  //This is 1-based!
		 
		for(int i=0,m=1;i<allFilamentsSize;i++,m++) {
			 al = filamentCoords.get(i);
			 alSize = al.size();
			 float c = 0;
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if(k==0) {
					 c = coords[4];
				 }
				 z = coords[2];
				 if(z>zVal) {
					 zVal=z;
					 highestZBlockIndex=i;
					 connectedTo = c;
				 }
			 } 
		}
		
		
		//next...set the highestZBlockIndex block to axon
		al = filamentCoords.get(highestZBlockIndex);
		alSize = al.size();
		for(int k=0;k<alSize;k++) {
			 coords = al.get(k);
			 coords[5] = 2;
			 al.set(k, coords);
		}
		
		//now traverse back until we get to soma (connectedTo=-1) and set to axon (2)
		while(connectedTo != -1) {
			int ind = (int)connectedTo - 1;
			al = filamentCoords.get(ind);
			alSize = al.size();
			for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if(k==0) {
					 connectedTo = coords[4];
				 }
				 coords[5] = 2;
				 al.set(k, coords);
			}
		}

	}
    

    
    /**
	 * calculate distances of each point fron beginning of line segment
	 */
	private void determineDistances_swc(ArrayList <ArrayList<float[]>> filamentCoords) {
		 int allFilamentsSize = filamentCoords.size();

		 int x1Pix=0,y1Pix=0,z1Pix=0;
		 int x2Pix,y2Pix,z2Pix;
		 float d;
		 int alSize;
		 ArrayList<float[]> al;
		 float[] coords;
		 for(int i=0;i<allFilamentsSize;i++) {
			 al = filamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 //if (r7CenterPointFound) {
					 //x2Pix = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 //y2Pix = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 //z2Pix = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 //}else {
					 //x2Pix = (int)Math.floor(coords[0]/resols[0]);
					 //y2Pix = (int)Math.floor(coords[1]/resols[1]);
					 //z2Pix = (int)Math.floor(coords[2]/resols[2]);
				 //}
				 
				 x2Pix = (int)Math.floor((coords[0]-finalImage.getOrigin()[0])/resols[0]);
				 y2Pix = (int)Math.floor((coords[1]-finalImage.getOrigin()[1])/resols[1]);
				 z2Pix = (int)Math.floor((coords[2]-finalImage.getOrigin()[2])/resols[2]);

				 if(k==0) {
					 x1Pix = x2Pix;
					 y1Pix = y2Pix;
					 z1Pix = z2Pix;
					 coords[3] = 0;
					 al.set(k, coords);
				 }else {
					d  = (float)MipavMath.distance(x1Pix, y1Pix, z1Pix, x2Pix, y2Pix, z2Pix, resols);
					coords[3] = d;
					al.set(k, coords);
				 }
			 }
		 }
	}
	
	
	
	
	
	
	
	
	
	 /**
	 * subsample
	 */
	private void subsample_swc(ArrayList <ArrayList<float[]>> filamentCoords, ArrayList <ArrayList<float[]>> newFilamentCoords) {
		 int allFilamentsSize = filamentCoords.size();
		 float x,y,z,d,c,a;
		 float xb,yb,zb,db;
		 float xn,yn,zn;
		 float dDiff1,dDiff2;
		 float ratio;
		 int alSize;
		 ArrayList<float[]> al;
		 ArrayList<float[]> newAl;
		 float[] newCoords;
		 float totalDistance;

		 //go through each segment
		 for(int i=0;i<allFilamentsSize;i++) {
			 al = filamentCoords.get(i);
			 alSize = al.size();
			 newAl = new ArrayList<float[]>();
			 //the 0th one stays as is
			 x = al.get(0)[0];
			 y = al.get(0)[1];
			 z = al.get(0)[2];
			 c = al.get(0)[4];
			 a = al.get(0)[5];
			 newCoords = new float[6];
			 newCoords[0] = x;
			 newCoords[1] = y;
			 newCoords[2] = z;
			 //in newCoords, this is now radius
			 newCoords[3] = -1;
			 newCoords[4] = c;
			 newCoords[5] = a;
			 newAl.add(newCoords);
			 
			 
			 
			 
			 
			 //first get total distance of the segment
			 totalDistance = al.get(alSize-1)[3];
			 loop:	for(float k=subsamplingDistance;k<=totalDistance;k=k+subsamplingDistance) {
						 for(int m=1;m<alSize-1;m++) {
							 x = al.get(m)[0];
							 y = al.get(m)[1];
							 z = al.get(m)[2];
							 d = al.get(m)[3];
							 c = al.get(m)[4];
							 a = al.get(m)[5];
							 if(k==d) {
								 //highly unlikely...but just in case
								 newCoords = new float[6];
								 newCoords[0] = x;
								 newCoords[1] = y;
								 newCoords[2] = z;
								 newCoords[3] = 0;
								 newCoords[4] = c;
								 newCoords[5] = a;
								 newAl.add(newCoords);
								 continue loop;
							 }else if(k<d) {
								 xb = al.get(m-1)[0];
								 yb = al.get(m-1)[1];
								 zb = al.get(m-1)[2];
								 db = al.get(m-1)[3];
								 dDiff1 = k-db;
								 dDiff2 = d-db;
								 ratio = dDiff1/dDiff2;
								 xn = xb + (ratio*(x-xb));
								 yn = yb + (ratio*(y-yb));
								 zn = zb + (ratio*(z-zb));
								 newCoords = new float[6];
								 newCoords[0] = xn;
								 newCoords[1] = yn;
								 newCoords[2] = zn;
								 newCoords[3] = -1;
								 newCoords[4] = c;
								 newCoords[5] = a;
								 newAl.add(newCoords);
								 continue loop;
							 }
						 } 
					 } //end loop:
			 
			//the last one stays as is
			 x = al.get(alSize-1)[0];
			 y = al.get(alSize-1)[1];
			 z = al.get(alSize-1)[2];
			 c = al.get(alSize-1)[4];
			 a = al.get(alSize-1)[5];
			 newCoords = new float[6];
			 newCoords[0] = x;
			 newCoords[1] = y;
			 newCoords[2] = z;
			 newCoords[3] = -1;
			 newCoords[4] = c;
			 newCoords[5] = a;
			 newAl.add(newCoords);
			 
			 
			 newFilamentCoords.add(newAl);
		 }
		 
	}
	
	
	
	/**
	 * Determines connectivity for all points
	 */
	private void determineConnectivity2_swc(ArrayList <ArrayList<float[]>> newFilamentCoords) {
		
		int newFilamentsSize = newFilamentCoords.size();
		int alSize;
		ArrayList<float[]> al,al2;
		float[] coords;
		float c;
		int count = 1;
		c=0;
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			
			 
			 for(int k=0;k<alSize;k++) {
				 
				 coords = al.get(k);
				 if(k==0) {
					 c = coords[4];
					 continue;
				 }else if(k==1) {
					 
					 if(i==0) {
						 coords[4] = count;
						 al.set(k, coords);
					 }else{
						 //get the last emements connectivity of c-1
						 al2 = newFilamentCoords.get((int)c-1);
						 //System.out.println("********" + al2.size());
						 coords[4] = al2.get(al2.size()-1)[4] + 1;
						 al.set(k, coords);
						 //counter = al2.size() + 1;
						 //counter = al2.size() +1;
						 
					 }
				 }else{
					 
					 coords[4] = count;
					 al.set(k, coords);
					 
					 
				 }

				 count = count + 1;
				
			 }
			 
		}
		
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	 /**
     * Dtermeines radius based upon a user-input threshold
     * Sphere keeps growing till threshold is hit
     */
    private void determineRadiiThreshold_swc(ArrayList <ArrayList<float[]>> newFilamentCoords) {
		System.out.println("determineRadii");
		///////////////////////////  
		/*int[] extents = {512, 512, 512};
        maskImage = new ModelImage(ModelStorageBase.UBYTE, extents, "maskImage_swc_threshold");
        for (int i = 0; i < maskImage.getExtents()[2]; i++) {
        	maskImage.setResolutions(i, resols);
        }
        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[maskImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(maskImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(finalImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(finalImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(finalImage.getExtents());
            fileInfoBases[i].setOrigin(finalImage.getFileInfo()[0].getOrigin());

        }
        maskImage.setFileInfo(fileInfoBases);*/
        /////////////////////////////////////
		

		float resX = resols[0];
		float resY = resols[1];
		float resZ = resols[2];
		
		float increaseRadiusBy = resZ;
		
		int newFilamentsSize = newFilamentCoords.size();
		System.out.println(newFilamentsSize);
		int alSize;
		ArrayList<float[]> al;
		float[] coords;
		int xCenterPixel,yCenterPixel,zCenterPixel;
		float radius;
		float radiusSquared;
		float xDist,yDist,zDist;
		float distance;
		int xStart,yStart,zStart;
		int xEnd,yEnd,zEnd;
		float greenValue;
		
		System.out.println("aaa");
		//intitilaize all raii to 0 first
		for(int i=0;i<newFilamentsSize;i++) {
			
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 coords[3] = 0;
				 al.set(k, coords);
				 
			 }
		}
		
		
		
		

		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();

			 for(int k=0;k<alSize;k++) {

				 coords = al.get(k);
				 //if(r7CenterPointFound) {
					 //xCenterPixel = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 //yCenterPixel = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 //zCenterPixel = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 //}else {
					 //xCenterPixel = (int)Math.floor(coords[0]/resols[0]);
					// yCenterPixel = (int)Math.floor(coords[1]/resols[1]);
					 //zCenterPixel = (int)Math.floor(coords[2]/resols[2]);
				 //}
				 
				 xCenterPixel = (int)Math.floor((coords[0]-finalImage.getOrigin()[0])/resols[0]);
				 yCenterPixel = (int)Math.floor((coords[1]-finalImage.getOrigin()[1])/resols[1]);
				 zCenterPixel = (int)Math.floor((coords[2]-finalImage.getOrigin()[2])/resols[2]);

				 //expand radius..start with increaseRadiusBy and increse
				 loop:		for(radius=increaseRadiusBy;;radius=radius+increaseRadiusBy) {
								//System.out.println("** " + radius);
								 radiusSquared = radius * radius;  //we will work with radius squared...that way we dont have to do SqrRt down in the for loops
								 
								 xStart = xCenterPixel - Math.round(radius/resX);
								 xStart = xStart - 1;
								 yStart = yCenterPixel - Math.round(radius/resY);
								 yStart = yStart - 1;
								 zStart = zCenterPixel - Math.round(radius/resZ);
								 zStart = zStart - 1;
								 
								 xEnd = xCenterPixel + Math.round(radius/resX);
								 xEnd = xEnd + 1;
								 yEnd = yCenterPixel + Math.round(radius/resY);
								 yEnd = yEnd + 1;
								 zEnd = zCenterPixel + Math.round(radius/resZ);
								 zEnd = zEnd + 1;
							
								 for(int z=zStart;z<=zEnd;z++) {
									 zDist = ((z-zCenterPixel)*(resZ)) * ((z-zCenterPixel)*(resZ));
									 for(int y=yStart;y<=yEnd;y++) {
										 yDist = ((y-yCenterPixel)*(resY)) * ((y-yCenterPixel)*(resY));
										 for(int x=xStart;x<=xEnd;x++) {
											 xDist = ((x-xCenterPixel)*(resX)) * ((x-xCenterPixel)*(resX));
											 distance = xDist + yDist + zDist;

											 if(distance <= radiusSquared) {
												 //this means we have a valid pixel in the sphere
												 //first check to see of x,y,z are in bounds
												 /*if(x<0 || x>=finalImage.getExtents()[0] || y<0 || y>=finalImage.getExtents()[1] || z<0 || z>=finalImage.getExtents()[2]) {
													 continue;
												 }
												 greenValue = finalImage.getFloatC(x, y, z, 2);*/
												 
												 
												 if(x<0 || x>=greenImage.getExtents()[0] || y<0 || y>=greenImage.getExtents()[1] || z<0 || z>=greenImage.getExtents()[2]) {
													 continue;
												 }
												 greenValue = greenImage.getFloat(x, y, z);
												 
												 
												 System.out.println("green value is " + greenValue	);
												 
												 if(greenValue <= greenThreshold) {
													 //this means we have exceeded the radius
													 //break the loop:  and move on to next point
													 //store the radius....but make sure you store radius-increaseRadiusBy since this one has exceeded
													 coords[3] = radius-increaseRadiusBy;
													 al.set(k, coords);
													 break loop;
												 }else {
													 ///////////////////////////////////////////////////////////
													//maskImage.set(x, y, z, 100);
												 }
											 }
											 
										 }
										 
									 }
								 }
							 } //end loop:
			 }
		}
		System.out.println("ccc");
		
		//some radii might still be at 0 because the threshold was already met...in this case set the
		//radius to the "increaseRadiusBy" step size
		float r;
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 r = coords[3];
				 if(r == 0) {
					 coords[3] = increaseRadiusBy;
					 al.set(k, coords);
				 }
			 }
		}
		
		System.out.println("ddd");
		
		
	}
	
	
    
    
    
    
    
    
    
    /**
	 * outputs the swc file
	 */
	private void output_swc(String filename,ArrayList <ArrayList<float[]>> newFilamentCoords) {
		try {
			
	        final File newSurfaceFile = new File(filamentFile.getParent() + File.separator  + filename);
	        final FileWriter fw = new FileWriter(newSurfaceFile);
	        final BufferedWriter bw = new BufferedWriter(fw);

			int newFilamentsSize = newFilamentCoords.size();
			int alSize;
			ArrayList<float[]> al;
			float[] coords;
			float x,y,z,r,c,a;
			int cInt,aInt;
			int counter = 1;
			for(int i=0;i<newFilamentsSize;i++) {
				 al = newFilamentCoords.get(i);
				 alSize = al.size();
				 for(int k=0;k<alSize;k++) {
					 if(k==0 && i!=0) {
						 continue;
					 }
					 coords = al.get(k);
	
					 x = coords[0];
					 y = coords[1];
					 z = coords[2];
					 r = coords[3];
					 c = coords[4];
					 a = coords[5];
					 cInt = (int)c;
					 aInt = (int)a;
					 //System.out.println(counter + " " + aInt + " " + x + " " + y + " " + z + " " + r + " " + cInt) ;
					 bw.write(counter + " " + aInt + " " + x + " " + y + " " + z + " " + r + " " + cInt);
					 bw.newLine();
					 counter++;
					 //System.out.println("   " + Math.abs(Math.round(x/resols[0])) + " " + Math.abs(Math.round(y/resols[1])) + " " + Math.abs(Math.round(z/resols[2])));
				 }
				 
			}
			
		
			bw.close();
		}catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	

}
