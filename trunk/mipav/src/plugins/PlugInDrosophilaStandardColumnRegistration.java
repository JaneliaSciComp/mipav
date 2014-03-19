import java.awt.GraphicsEnvironment;
import java.io.File;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.CommandLineParser;
import gov.nih.mipav.view.MipavUtil;


public class PlugInDrosophilaStandardColumnRegistration implements PlugInGeneric, CommandLineParser {

	
	private PlugInHeadlessDrosophilaStandardColumnRegistration headless;
	
	
	public PlugInDrosophilaStandardColumnRegistration() {
		
	}

	
	

	public void run() {
		
		if(GraphicsEnvironment.isHeadless()) {

			headless.callAlgorithm();
			
		}else {
			new PlugInDialogDrosophilaStandardColumnRegistration(false);
		}
		
		

	}

	
	  
	
	
	
	public int parseArguments(String[] args, int initArg) {
		System.out.println("parsing arguments");
		headless = new PlugInHeadlessDrosophilaStandardColumnRegistration();
		int i = 0;
		for(i=0;i<args.length;i++) {
			System.out.println(args[i]);
			String varName = args[i];
			i = i + 1;
			String value = args[i];
			if( varName.equalsIgnoreCase("-image")) {
				FileIO fileIO = new FileIO();
				String replaced = value.replace("\\", File.separator);
				String name = replaced.substring(replaced.lastIndexOf(File.separator)+1, replaced.length());
				String dir = replaced.substring(0,replaced.lastIndexOf(File.separator));
				ModelImage img = fileIO.readImage(name, dir + File.separator, true, null);
				headless.setNeuronImage(img);

				float[] resols = headless.neuronImage.getResolutions(0);
				headless.setResols(resols);
			
				
			}else if(varName.equalsIgnoreCase("-pointsFile")) {
				
				String replaced = value.replace("\\", File.separator);
				File pointsFile = new File(replaced);
				headless.setPointsFile(pointsFile);
				if(!headless.readPointsFile(headless.pointsFile)) {
		    		MipavUtil.displayError("Error parsing points file");
		    		return -1;
				}

			}else if(varName.equalsIgnoreCase("-filamentFile")) {
				
				
				String replaced = value.replace("\\", File.separator);
				File surfaceFile = new File(replaced);
				headless.setSurfaceFile(surfaceFile);
		    	if(!headless.readSurfaceFile(headless.surfaceFile)) {
		    		
		    		MipavUtil.displayError("Error parsing surface file");

		    		return -1;
		    		
		    	}else {
		    		if(headless.determineIfProperlyConnected()) {
		    			headless.createCityBlockImage();
		    		}else {
		    			MipavUtil.displayError("Error parsing surface file");

		        		return -1;
		    		}
		    		
		    		

		    		
		    	}
				
				
			}else if(varName.equalsIgnoreCase("-filamentSampling")) {
				float sampl = Float.valueOf(value);
				headless.setSamplingRate(sampl);
				
				
			}else if(varName.equalsIgnoreCase("-invertIV")) {
				
				if(value.equalsIgnoreCase("flipNone")) {
					headless.setFlipX(false);
					headless.setFlipY(false);
					headless.setFlipZ(false);
				}else if (value.equalsIgnoreCase("flipX")) {
					headless.setFlipX(true);
					headless.setFlipY(false);
					headless.setFlipZ(false);
				}else if (value.equalsIgnoreCase("flipY")) {
					headless.setFlipX(false);
					headless.setFlipY(true);
					headless.setFlipZ(false);
				}else if (value.equalsIgnoreCase("flipZ")) {
					headless.setFlipX(false);
					headless.setFlipY(false);
					headless.setFlipZ(true);
				}else if (value.equalsIgnoreCase("flipXY")) {
					headless.setFlipX(true);
					headless.setFlipY(true);
					headless.setFlipZ(false);
				}else if (value.equalsIgnoreCase("flipXZ")) {
					headless.setFlipX(true);
					headless.setFlipY(false);
					headless.setFlipZ(true);
				}else if (value.equalsIgnoreCase("flipYZ")) {
					headless.setFlipX(false);
					headless.setFlipY(true);
					headless.setFlipZ(true);
				}else if (value.equalsIgnoreCase("flipXYZ")) {
					headless.setFlipX(true);
					headless.setFlipY(true);
					headless.setFlipZ(true);
				}
				
			}else if(varName.equalsIgnoreCase("-numPoints")) {
				
				/*if(value.equalsIgnoreCase("27_points")) {
					headless.setNumPoints(PlugInDialogDrosophilaStandardColumnRegistration._27POINTS);
				}else if(value.equalsIgnoreCase("75_points")) {
					headless.setNumPoints(PlugInDialogDrosophilaStandardColumnRegistration._75POINTS);
				}else if(value.equalsIgnoreCase("147_points")) {
					headless.setNumPoints(PlugInDialogDrosophilaStandardColumnRegistration._147POINTS);
				}*/
				
				
				if(value.equalsIgnoreCase("27A_points")) {
					headless.setNumPointsString(PlugInDialogDrosophilaStandardColumnRegistration._27APOINTS);
				}else if(value.equalsIgnoreCase("75A_points")) {
					headless.setNumPointsString(PlugInDialogDrosophilaStandardColumnRegistration._75APOINTS);
				}
				
			}else if(varName.equalsIgnoreCase("-orientation")) {
				
				if(value.equalsIgnoreCase("lvrd")) {
					headless.setRvld(false);
				}else if(value.equalsIgnoreCase("rvld")) {
					headless.setRvld(true);
				}
				
			}else if(varName.equalsIgnoreCase("-regsitration")) {
				
				if(value.equalsIgnoreCase("rigidTPS")) {
					headless.setRigidOnly(false);
				}else if(value.equalsIgnoreCase("rigidOnly")) {
					headless.setRigidOnly(true);
				}
				
			}else if(varName.equalsIgnoreCase("-swcRadius")) {
				
				
				float greenThreshold = Float.valueOf(value).floatValue();
				headless.setGreenThreshold(greenThreshold);
				
				
			}else if(varName.equalsIgnoreCase("-swcSubsampDist")) {
			
				float subsamplingDistance = Float.valueOf(value).floatValue();
				headless.setSubsamplingDistance(subsamplingDistance);
				boolean doSWC = true;
				headless.setDoSWC(doSWC);
			}
			
			
			
			
			
			
		}
		
		
		
		
		return i;
	}

}
