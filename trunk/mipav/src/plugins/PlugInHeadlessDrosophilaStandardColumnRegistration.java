import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.TreeMap;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.LineBorder;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ScrollCorrector;


public class PlugInHeadlessDrosophilaStandardColumnRegistration implements AlgorithmInterface {

	

	
	/** image to register to standard column **/
	public ModelImage neuronImage,cityBlockImage;
	
	/** points file **/
	public File pointsFile, surfaceFile;

    
    /** handle to algorithm **/
    public PlugInAlgorithmDrosophilaStandardColumnRegistration alg;
    
    /** points collection **/
    public TreeMap<Integer, float[]> pointsMap;
    
    /** coords of filament **/
    public ArrayList <ArrayList<float[]>> allFilamentCoords = new ArrayList <ArrayList<float[]>>();

    /** resolutions **/
    public float[] resols;

    /** boolean for doing swc **/
    public boolean doSWC = false;

   /** green value radius threshold **/
   public float greenThreshold;
   
	/** subsampling distance **/
   public float subsamplingDistance;
   
   private boolean plugInCompleted = false;
   
   /*public static final int _27POINTS = 27;
   public static final int _75POINTS = 75;
   public static final int _147POINTS = 147;*/
   
   
   public static final String _27POINTS = "27";
   public static final String _75POINTS = "75";
   public static final String _147POINTS = "147";
   public static final String _27APOINTS = "27A";
   public static final String _75APOINTS = "75A";
   
   private String numPointsString;
   
   private float samplingRate;
   
   private boolean flipX, flipY, flipZ;
   
   private boolean rvld;
   
   private boolean rigidOnly;
   
   

	

	/**
	 * constructor
	 */
	public PlugInHeadlessDrosophilaStandardColumnRegistration() {
		
	}


	
	public  void setNumPointsString(String numPointsString) {
		this.numPointsString = numPointsString;
	}
	
	

	
	
	/**
	 * reads surface file
	 * @param surfaceFile
	 * @return
	 */
	public boolean readSurfaceFile(File surfaceFile) {
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
							
							//we will get the coords in proper mipav space in the method, determineIfProperlyConnected
							/*float x = coord_x/resols[0];
							float y = coord_y/resols[1];
							float z = coord_z/resols[2];*/
							  
							float[] coords = {coord_x,coord_y,coord_z,0};
							
							filamentCoords.add(coords);
						}
					}
					allFilamentCoords.add(filamentCoords);
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
	
	
	
	public boolean determineIfProperlyConnected() {
		boolean success = true;
		int[] connectednessList = new int[allFilamentCoords.size()];
		//initialize all to 0
		for(int i=0;i<connectednessList.length;i++) {
			connectednessList[i] = 0;
		}
		
		
		
		
		//set first one to -1
		connectednessList[0] = -1;
		int alMatchSize;
		ArrayList<float[]> al;
		ArrayList<float[]> alMatch;
		float[] coordsMatch = new float[3];
		float[] coords;
		
		for(int i=1;i<allFilamentCoords.size();i++) {
			
			
			 al = allFilamentCoords.get(i);
			 coords = al.get(0);
			
			int k;
			 
			 for(k=0;k<i;k++) {
				 alMatch = allFilamentCoords.get(k);
				 alMatchSize = alMatch.size();
				 
				 coordsMatch[0] = alMatch.get(alMatchSize-1)[0];
				 coordsMatch[1] = alMatch.get(alMatchSize-1)[1];
				 coordsMatch[2] = alMatch.get(alMatchSize-1)[2];
				 
				 if(coords[0]==coordsMatch[0] && coords[1]==coordsMatch[1] && coords[2]==coordsMatch[2]) {

					 connectednessList[i] = k+1;


					 break;
				 }
				 
				 
				 
			 }
			
			
		}
		
		
		//if any in connectednessList is 0, then this is not a properly connected iv file
		int index = 0;
		for(int i=0;i<connectednessList.length;i++) {
			if(connectednessList[i] == 0) {
				success = false;
				index = i;
				break;
			}
		}
		
		if(success == false) {
			al = allFilamentCoords.get(index);
			coords = al.get(0);
			String coordsString = coords[0] + "," + coords[1] + "," + coords[2];
			MipavUtil.displayError("IV file is not properly connected: the block of points starting with " + coordsString + " is not connected to anything");
			System.out.println("IV file is not properly connected: the block of points starting with " + coordsString + " is not connected to anything");
		}else {
			//get the coords in proper mipav space!!!
			for(int i=0;i<allFilamentCoords.size();i++) {
				 al = allFilamentCoords.get(i);
				 for(int k=0;k<al.size();k++) {
					 float[] coords2 = al.get(k);
					 
					 
					 float x = coords2[0]/resols[0];
					 float y = coords2[1]/resols[1];
					 float z = coords2[2]/resols[2];
					 
					 coords2[0] = x;
					 coords2[1] = y;
					 coords2[2] = z;
					 
					 al.set(k, coords2);
					 
					 
					 
					 
				 }
				 
				 
				 
			}
			
		}
		
		
		
		return success;
	}
	
	

	
	public synchronized void setRigidOnly(boolean rigidOnly) {
		this.rigidOnly = rigidOnly;
	}



	public synchronized void setSamplingRate(float samplingRate) {
		this.samplingRate = samplingRate;
	}



	/**
	 * call algorithm
	 */
	public void callAlgorithm() {
		alg = new PlugInAlgorithmDrosophilaStandardColumnRegistration(neuronImage,pointsMap,allFilamentCoords,surfaceFile,samplingRate,cityBlockImage,pointsFile,null,flipX, flipY, flipZ,greenThreshold,subsamplingDistance,rigidOnly,doSWC,rvld,numPointsString);
		alg.addListener(this);
		alg.run();

		
	}
	
	
	/**
	 * read points file
	 * @param pointsFile
	 * @return
	 */
	public boolean readPointsFile(File pointsFile) {
		RandomAccessFile raFile = null;
		try {
			pointsMap = new TreeMap<Integer, float[]>();
            raFile = new RandomAccessFile(pointsFile, "r");
            String[] arr;
            //String[] arr2;
            String line;
            float x,y,z;
            int counter = 1;
            while((line = raFile.readLine()) != null) {
            		arr = line.trim().split(",");
            		if(arr.length == 4) {
            			if(arr[0].trim().equals("") && arr[1].trim().equals("") && arr[2].trim().equals("")) {
            				pointsMap.put(new Integer(counter), null);
	            		}else {
	                    	x = new Float(arr[0]).floatValue();
	                    	y = new Float(arr[1]).floatValue();
	                    	z = new Float(arr[2]).floatValue();
	                    	float[] f = {x,y,z};
	                    	pointsMap.put(new Integer(counter), f);
	            		}
                    }else {
                    	return false;
                    	/*if(arr.length == 1) {
                    		pointsMap.put(new Integer(counter), null);
                    	}else {
                    		return false;
                    	}*/
                    }
            		counter++;
            	
            }
            raFile.close();
            if(pointsMap.size() == 27 || pointsMap.size() == 75 || pointsMap.size() == 147) {
            	return true;
            }else {
            	return false;
            }
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
	}
	
	
	/**
	 * creates city block image
	 */
	public void createCityBlockImage() {
		
		int[] extents = {512,512,512};
        cityBlockImage = new ModelImage(ModelImage.UBYTE, extents,"cityBlockImage");
        float[] cityBlockImageResols = new float[3];
        cityBlockImageResols[0] = neuronImage.getResolutions(0)[0];
        cityBlockImageResols[1] = neuronImage.getResolutions(0)[1];
        cityBlockImageResols[2] = neuronImage.getResolutions(0)[2];
		for(int i=0;i<cityBlockImage.getExtents()[2];i++) {
			cityBlockImage.setResolutions(i, cityBlockImageResols);
		}
		
		FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[cityBlockImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(neuronImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(neuronImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(neuronImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());

        }

        cityBlockImage.setFileInfo(fileInfoBases);
        byte[] cityBlockImageBuffer = new byte[512*512*512];
        for(int i=0;i<cityBlockImageBuffer.length;i++) {
        	cityBlockImageBuffer[i] = 100;	
        }
        try {
        	cityBlockImage.importData(0, cityBlockImageBuffer, true);
        } catch (IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }
        
        
         int x1,y1,z1;
         
         ArrayList<int[]> zeroCoords = new ArrayList<int[]>();

		 
		 int allFilamentsSize = allFilamentCoords.size();
		 int alSize;
		 ArrayList<float[]> al;
		 float[] coords;
		 for(int i=0;i<allFilamentsSize;i++) {
			 al = allFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 x1 = Math.round(coords[0]);
				 y1 = Math.round(coords[1]);
				 z1 = Math.round(coords[2]);
				 int[] arr = {x1,y1,z1};
		         zeroCoords.add(arr);
		         cityBlockImage.set(x1,y1,z1,0);
		         
			 }
		 }

		 
		 ArrayList<int[]> oneCoords = new ArrayList<int[]>();
		 for(int i=0;i<zeroCoords.size();i++) {
			int[] zeroCoord = zeroCoords.get(i);
			int zeroX = zeroCoord[0];
			int zeroY = zeroCoord[1];
			int zeroZ = zeroCoord[2];
			
			int zeroXPlus1 = zeroX + 1;
			int zeroXMinus1 = zeroX - 1;
			int zeroYPlus1 = zeroY + 1;
			int zeroYMinus1 = zeroY - 1;
			int zeroZPlus1 = zeroZ + 1;
			int zeroZMinus1 = zeroZ - 1;
			
			
			
			if ((cityBlockImage.get(zeroXPlus1, zeroY, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroXPlus1,zeroY,zeroZ,1);
				int[] arr = {zeroXPlus1,zeroY,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroXMinus1, zeroY, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroXMinus1,zeroY,zeroZ,1);
				int[] arr = {zeroXMinus1,zeroY,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroYPlus1, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroYPlus1,zeroZ,1);
				int[] arr = {zeroX,zeroYPlus1,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroYMinus1, zeroZ)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroYMinus1,zeroZ,1);
				int[] arr = {zeroX,zeroYMinus1,zeroZ};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroY, zeroZPlus1)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroY,zeroZPlus1,1);
				int[] arr = {zeroX,zeroY,zeroZPlus1};
				oneCoords.add(arr);
			}
			if ((cityBlockImage.get(zeroX, zeroY, zeroZMinus1)).byteValue() == 100) {
				cityBlockImage.set(zeroX,zeroY,zeroZMinus1,1);
				int[] arr = {zeroX,zeroY,zeroZMinus1};
				oneCoords.add(arr);
			}
		 }
		 
		 
		 
		 
		 

		 for(int i=0;i<oneCoords.size();i++) {
				int[] oneCoord = oneCoords.get(i);
				int oneX = oneCoord[0];
				int oneY = oneCoord[1];
				int oneZ = oneCoord[2];
				
				int oneXPlus1 = oneX + 1;
				int oneXMinus1 = oneX - 1;
				int oneYPlus1 = oneY + 1;
				int oneYMinus1 = oneY - 1;
				int oneZPlus1 = oneZ + 1;
				int oneZMinus1 = oneZ - 1;
				
				
				
				if ((cityBlockImage.get(oneXPlus1, oneY, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneXPlus1,oneY,oneZ,2);
				}
				if ((cityBlockImage.get(oneXMinus1, oneY, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneXMinus1,oneY,oneZ,2);
				}
				if ((cityBlockImage.get(oneX, oneYPlus1, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneYPlus1,oneZ,2);
				}
				if ((cityBlockImage.get(oneX, oneYMinus1, oneZ)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneYMinus1,oneZ,2);
				}
				if ((cityBlockImage.get(oneX, oneY, oneZPlus1)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneY,oneZPlus1,2);
				}
				if ((cityBlockImage.get(oneX, oneY, oneZMinus1)).byteValue() == 100) {
					cityBlockImage.set(oneX,oneY,oneZMinus1,2);
				}
		 }

		 cityBlockImage.calcMinMax();
                                  
	}
        
		
		

	
	
	
	
	
	public synchronized void setNeuronImage(ModelImage neuronImage) {
		this.neuronImage = neuronImage;
	}



	public synchronized void setCityBlockImage(ModelImage cityBlockImage) {
		this.cityBlockImage = cityBlockImage;
	}



	public synchronized void setPointsFile(File pointsFile) {
		this.pointsFile = pointsFile;
	}



	public synchronized void setSurfaceFile(File surfaceFile) {
		this.surfaceFile = surfaceFile;
	}



	public synchronized void setPointsMap(TreeMap<Integer, float[]> pointsMap) {
		this.pointsMap = pointsMap;
	}



	public synchronized void setResols(float[] resols) {
		this.resols = resols;
	}



	public synchronized void setDoSWC(boolean doSWC) {
		this.doSWC = doSWC;
	}



	public synchronized void setGreenThreshold(float greenThreshold) {
		this.greenThreshold = greenThreshold;
	}



	public synchronized void setSubsamplingDistance(float subsamplingDistance) {
		this.subsamplingDistance = subsamplingDistance;
	}



	public synchronized void setRvld(boolean rvld) {
		this.rvld = rvld;
	}



	public synchronized void setFlipX(boolean flipX) {
		this.flipX = flipX;
	}



	public synchronized void setFlipY(boolean flipY) {
		this.flipY = flipY;
	}



	public synchronized void setFlipZ(boolean flipZ) {
		this.flipZ = flipZ;
	}



	public synchronized boolean isPlugInCompleted() {
		return plugInCompleted;
	}

	public synchronized void setPlugInCompleted(boolean plugInCompleted) {
		this.plugInCompleted = plugInCompleted;
	}

	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			
			 setPlugInCompleted(true);
		}
		
	}
	
	
	


}
