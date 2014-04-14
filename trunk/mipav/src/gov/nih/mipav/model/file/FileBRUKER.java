package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.Preferences;

import java.io.*;
import java.util.ArrayList;


/**
 * Reads a BRUKER file by first reading in the d3proc header file, second the reco header file, third the acqp file int the 
 * same directory or up one or two two parent directories, and finally the 2dseq binary file.
 */

public class FileBRUKER extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoBRUKER fileInfo;
    
    private FileInfoBRUKER fileInfoCopy;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private boolean foundEOF = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int imageSlice;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** The preferred image name, used if inversion time exists. */
    private String prefImageName = null;
    
    private int numVolumes = -1;
    
    private DTIParameters dtiParams = null;
    
    private double bMatrixVals[][] = null;
    
    private double gradients[][] = null;
    
    private double bValues[] = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * BRUKER reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileBRUKER(String fileName, String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
        
        File f = new File(fileDir + fileName); //file separator already included
        String tryFileDir = null;
        if(!f.exists()) {  //since BRUKER is not a specific file, the user may enter one directory to high,
                           //this method does a search by depth to 3 in looking for the d3proc file
            tryFileDir = searchChildDir(new File(fileDir));
        }
        
        if(tryFileDir != null) {
            this.fileDir = tryFileDir;
        }
    }
    
    /**
     * Utility method for searching for the d3proc file that indicates a BRUKER file has been found,
     * note method is recursive to a maximum level of 3.
     */
    private String searchChildDir(File currentDir) {
        System.out.println("Searching: "+currentDir.getAbsolutePath()+File.separator +fileName);
        if(!new File(currentDir.getAbsolutePath() + File.separator + fileName).exists()) {
            ArrayList<String> subDir = childDir(currentDir);
            for(String dir : subDir) {
                if(new File(dir + File.separator + fileName).exists()) {
                    System.out.println("Returning "+dir+File.separator);
                    return dir+File.separator;
                }
            }
            for(String dir : subDir) {
                String str = searchChildDir(new File(dir));
                System.out.println("Returning "+str);
                return str;
            }
        }
        return currentDir.getAbsolutePath()+File.separator;
    }

    /**
     * Returns an ArrayList of all the names of subdirectories of the 
     * directory denoted by <code>currentDir</code>.
     */
    private ArrayList<String> childDir(File currentDir) {
        ArrayList<String> subDir = new ArrayList<String>();
        String[] fileList = currentDir.list();
        for(int i=0; i<fileList.length; i++) {
            if(new File(currentDir.getAbsolutePath() + File.separator + fileList[i]).isDirectory()) {
                subDir.add(currentDir.getAbsolutePath() + File.separator + fileList[i]);
                System.out.println("Adding "+currentDir.getAbsolutePath() + File.separator + fileList[i]);
            }
        }
        
        return subDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
    	int i;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileInfoCopy = null;
        file = null;
        image = null;
        imgResols = null;
        LUT = null;
        bValues = null;
        if (gradients != null) {
        	for (i = 0; i < gradients.length; i++) {
        		gradients[i] = null;
        	}
        	gradients = null;
        }
        if (bMatrixVals != null) {
        	for (i = 0; i < bMatrixVals.length; i++) {
        		bMatrixVals[i] = null;
        	}
        	bMatrixVals = null;
        }
        dtiParams = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }

    /**
     * returns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }
    
    /**
     * 
     * @return
     */
    public DTIParameters getDTIParameters() {
    	return dtiParams;
    }
    
    /**
     * Reads the optional method file that may be part of the Bruker scan.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readMethod() throws IOException {
        String lineString = null;
        String[] parseString;
        boolean okay;
        int numVars;
        int numFound;
        double bMat[][][] = null;
        boolean bMatOkay;
        int index0;
        int index1;
        int index2;
        boolean gradientsOkay;
        boolean bValuesOkay;
        int i;
        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$PVM_InversionTime")) {

                if (parseString.length == 2) {

                    try {
                        Double invTime = Double.valueOf(parseString[1]);
                        fileInfo.setInversionTime(invTime.doubleValue());
                        prefImageName = "img:invTime:"+invTime.intValue();
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Inversion time for "+fileName+" could not be read.\n", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_InversionTime has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_DwBMat")) {
            	okay = true;
            	if (parseString.length == 6) {
	                if (parseString[1].equals("(")) {
	                	Preferences.debug("For PVM_DwBMat parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
	                }
	                else
	                {
	                	Preferences.debug("For PVM_DwBMat parseString[1] unexpectedly == " + parseString[1] + "\n", 
	                			Preferences.DEBUG_FILEIO);
	                	okay = false;
	                }
	                if (okay) {
		                if (parseString[2].endsWith(",")) {
		                    numVolumes = Integer.valueOf(parseString[2].substring(0,parseString[2].length()-1));
		                    Preferences.debug("For PVM_DwBMat numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
		                }
		                else {
		                	Preferences.debug("For PVM_DwBMat parseString[2] unexpectedly == " + parseString[2] + "\n",
		                			Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }
	                }
	                if (okay) {
		                if (parseString[3].equals("3,")) {
		                    Preferences.debug("For PVM_DwBMat parseString[3] equals '3,', as expected\n", Preferences.DEBUG_FILEIO);			
		                }
		                else {
		                	Preferences.debug("For PVM_DwBMat parseString[3] unexpectedly == " + parseString[3] + "\n",
		                			          Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }
	                }
	                if (okay) {
	                	if (parseString[4].equals("3")) {
		                    Preferences.debug("For PVM_DwBMat parseString[4] equals 3, as expected\n", Preferences.DEBUG_FILEIO);			
		                }
		                else {
		                	Preferences.debug("For PVM_DwBMat parseString[4] unexpectedly == " + parseString[4] + "\n",
		                			          Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }	
	                }
	                if (okay) {
	                    if (parseString[5].equals(")")) {
	                        Preferences.debug("For PVM_DwMat parseString[5] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
	                    }
	                    else {
	                    	Preferences.debug("For PVM_DwBMat parseString[5] unexpectedly == " + parseString[5] + "\n",
	                    			Preferences.DEBUG_FILEIO);
		                	okay = false;	
	                    }
	                }
            	}
            	else {
            		Preferences.debug("For PVM_DwBMat parseString.length unexpectedly == " + parseString.length + "\n",
            				          Preferences.DEBUG_FILEIO);
            		okay = false;
            	}
            	if (okay) {
                    numVars = 9 * numVolumes;
            		numFound = 0;
            		bMat = new double[numVolumes][3][3];
            		index0 = 0;
            		index1 = 0;
            		index2 = 0;
            		bMatOkay = true;
            		while ((numFound < numVars) && (lineString != null) && bMatOkay) {
            			lineString = readLine();
            			if (lineString != null) {
            			    parseString = parse(lineString);
            			    for (i = 0; i < parseString.length && bMatOkay; i++) {
            			    	try {
            			    	    bMat[index0][index1][index2] = Double.valueOf(parseString[i]);
            			    	}
            			    	catch(NumberFormatException nfe) {
                                    Preferences.debug("bMat[" + index0 + "][" + index1 + "][" + index2 + "] could not be read.",
                                    		Preferences.DEBUG_FILEIO);
                                    bMatOkay = false;
                                }
            			    	if (bMatOkay) {
            			    		numFound++;
            			    		if (index2 < 2) {
            			    			index2++;
            			    		}
            			    		else if (index1 < 2) {
            			    			index2 = 0;
            			    			index1++;
            			    		}
            			    		else {
            			    			index2 = 0;
            			    			index1 = 0;
            			    			index0++;
            			    		}
            			    	}
            			    }
            			} // if (lineString != null)
            			else {
            				Preferences.debug("For PVM_DwBMat lineString == null while numFound == " + numFound + 
            						           " and numVars = " + numVars + "\n", Preferences.DEBUG_FILEIO);
            				bMatOkay = false;
            			}
            		} // while ((numFound < numVars) && (lineString != null) && bMatOkay)
            		if (numFound == numVars) {
            			for (i = 0; i < numVolumes && bMatOkay; i++) {
            			    if (bMat[i][0][1] != bMat[i][1][0]) {
            			    	Preferences.debug("bMat[" + i + "][0][1] = " + bMat[i][0][1] + " but bMat["+i+"][1][0] = " +
            			                           bMat[i][1][0] + "\n", Preferences.DEBUG_FILEIO);
            			    	bMatOkay = false;
            			    }
            			    if (bMat[i][0][2] != bMat[i][2][0]) {
            			    	Preferences.debug("bMat[" + i + "][0][2] = " + bMat[i][0][2] + " but bMat["+i+"][2][0] = " +
            			                           bMat[i][2][0] + "\n", Preferences.DEBUG_FILEIO);
            			    	bMatOkay = false;
            			    }
            			    if (bMat[i][1][2] != bMat[i][2][1]) {
            			    	Preferences.debug("bMat[" + i + "][1][2] = " + bMat[i][1][2] + " but bMat["+i+"][2][1] = " +
            			                           bMat[i][2][1] + "\n", Preferences.DEBUG_FILEIO);
            			    	bMatOkay = false;
            			    }
            			} // for (i = 0; i < numVolumes && bMatOkay; i++)
            			if (bMatOkay) {
            				if (dtiParams == null) {
            					dtiParams = new DTIParameters(numVolumes);
            				}
            				bMatrixVals = new double[numVolumes][6];
            				for (i = 0; i < numVolumes; i++) {
            					bMatrixVals[i][0] = bMat[i][0][0];
            					bMatrixVals[i][1] = bMat[i][0][1];
            					bMatrixVals[i][2] = bMat[i][0][2];
            					bMatrixVals[i][3] = bMat[i][1][1];
            					bMatrixVals[i][4] = bMat[i][1][2];
            					bMatrixVals[i][5] = bMat[i][2][2];
            			    }
            				dtiParams.setbMatrixVals(bMatrixVals);
            				Preferences.debug("Just did dtiParams.setbMatrixVals(bMatrixVals)\n", Preferences.DEBUG_FILEIO);
            			} // if (bMatOkay)
            		} // if (numFound == numVars)
            	} // if (okay)
            } // else if (parseString[0].equalsIgnoreCase("##$PVM_DwBMat"))
            else if (parseString[0].equalsIgnoreCase("##$PVM_DwGradVec")) {
            	okay = true;
            	if (parseString.length == 5) {
	                if (parseString[1].equals("(")) {
	                	Preferences.debug("For PVM_DwGradVec parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
	                }
	                else
	                {
	                	Preferences.debug("For PVM_DwGradVec parseString[1] unexpectedly == " + parseString[1] + "\n",
	                			Preferences.DEBUG_FILEIO);
	                	okay = false;
	                }
	                if (okay) {
		                if (parseString[2].endsWith(",")) {
		                    numVolumes = Integer.valueOf(parseString[2].substring(0,parseString[2].length()-1));
		                    Preferences.debug("For PVM_DwGradVec numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
		                }
		                else {
		                	Preferences.debug("For PVM_DwGradVec parseString[2] unexpectedly == " + parseString[2] + "\n",
		                			Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }
	                }
	                if (okay) {
	                	if (parseString[3].equals("3")) {
		                    Preferences.debug("For PVM_DwGradVec parseString[3] equals 3, as expected\n", Preferences.DEBUG_FILEIO);			
		                }
		                else {
		                	Preferences.debug("For PVM_DwGradVec parseString[3] unexpectedly == " + parseString[3] + "\n",
		                			          Preferences.DEBUG_FILEIO);
		                	okay = false;
		                }	
	                }
	                if (okay) {
	                    if (parseString[4].equals(")")) {
	                        Preferences.debug("For PVM_DwGradVec parseString[4] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
	                    }
	                    else {
	                    	Preferences.debug("For PVM_DwGradVec parseString[4] unexpectedly == " + parseString[4] + "\n",
	                    			Preferences.DEBUG_FILEIO);
		                	okay = false;	
	                    }
	                }
            	}
            	else {
            		Preferences.debug("For PVM_DwGradVec parseString.length unexpectedly == " + parseString.length + "\n",
            				          Preferences.DEBUG_FILEIO);
            		okay = false;
            	}
            	if (okay) {
                    numVars = 3 * numVolumes;
            		numFound = 0;
            		gradients = new double[numVolumes][3];
            		index0 = 0;
            		index1 = 0;
            		gradientsOkay = true;
            		while ((numFound < numVars) && (lineString != null) && gradientsOkay) {
            			lineString = readLine();
            			if (lineString != null) {
            			    parseString = parse(lineString);
            			    for (i = 0; i < parseString.length && gradientsOkay; i++) {
            			    	try {
            			    	    gradients[index0][index1] = Double.valueOf(parseString[i]);
            			    	}
            			    	catch(NumberFormatException nfe) {
                                    Preferences.debug("gradients[" + index0 + "][" + index1 + "] could not be read.",
                                    		Preferences.DEBUG_FILEIO);
                                    gradientsOkay = false;
                                }
            			    	if (gradientsOkay) {
            			    		numFound++;
            			    		if (index1 < 2) {
            			    			index1++;
            			    		}
            			    		else {
            			    			index1 = 0;
            			    			index0++;
            			    		}
            			    	}
            			    }
            			} // if (lineString != null)
            			else {
            				Preferences.debug("For PVM_DwGradVec lineString == null while numFound == " + numFound + 
            						           " and numVars = " + numVars + "\n", Preferences.DEBUG_FILEIO);
            				gradientsOkay = false;
            			}
            		} // while ((numFound < numVars) && (lineString != null) && gradientsOkay)
            		if (numFound == numVars) {
            			if (gradientsOkay) {
            				if (dtiParams == null) {
            					dtiParams = new DTIParameters(numVolumes);
            				}
            				dtiParams.setGradients(gradients);
            				Preferences.debug("Just did dtiParams.setGradients(gradients)\n", Preferences.DEBUG_FILEIO);
            			} // if (gradientsOkay)
            		} // if (numFound == numVars)
            	} // if (okay)
            } // else if (parseString[0].equalsIgnoreCase("##$PVM_DwGradVec"))
            else if (parseString[0].equalsIgnoreCase("##$PVM_DwEffBval")) {
            	okay = true;
            	if (parseString.length == 4) {
	                if (parseString[1].equals("(")) {
	                	Preferences.debug("For PVM_DwEffBval parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
	                }
	                else
	                {
	                	Preferences.debug("For PVM_DwEffBval parseString[1] unexpectedly == " + parseString[1] + "\n",
	                			Preferences.DEBUG_FILEIO);
	                	okay = false;
	                }
	                if (okay) {
		                numVolumes = Integer.valueOf(parseString[2]);
		                Preferences.debug("For PVM_DwEffBval numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
	                }
	                if (okay) {
	                    if (parseString[3].equals(")")) {
	                        Preferences.debug("For PVM_DwEffBval parseString[3] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
	                    }
	                    else {
	                    	Preferences.debug("For PVM_DwEffBval parseString[3] unexpectedly == " + parseString[3] + "\n",
	                    			Preferences.DEBUG_FILEIO);
		                	okay = false;	
	                    }
	                }
            	}
            	else {
            		Preferences.debug("For PVM_DwEffBval parseString.length unexpectedly == " + parseString.length + "\n",
            				          Preferences.DEBUG_FILEIO);
            		okay = false;
            	}
            	if (okay) {
            		numFound = 0;
            		bValues = new double[numVolumes];
            		bValuesOkay = true;
            		while ((numFound < numVolumes) && (lineString != null) && bValuesOkay) {
            			lineString = readLine();
            			if (lineString != null) {
            			    parseString = parse(lineString);
            			    for (i = 0; i < parseString.length && bValuesOkay; i++) {
            			    	try {
            			    	    bValues[numFound] = Double.valueOf(parseString[i]);
            			    	}
            			    	catch(NumberFormatException nfe) {
                                    Preferences.debug("bValues[" + numFound + "] could not be read.",
                                    		Preferences.DEBUG_FILEIO);
                                    bValuesOkay = false;
                                }
            			    	if (bValuesOkay) {
            			    		numFound++;
            			    	}
            			    }
            			} // if (lineString != null)
            			else {
            				Preferences.debug("For PVM_DwEffBval lineString == null while numFound == " + numFound + 
            						           " and numVolumes = " + numVolumes + "\n", Preferences.DEBUG_FILEIO);
            				bValuesOkay = false;
            			}
            		} // while ((numFound < numVOlumes) && (lineString != null) && bValuesOkay)
            		if (numFound == numVolumes) {
            			if (bValuesOkay) {
            				if (dtiParams == null) {
            					dtiParams = new DTIParameters(numVolumes);
            				}
            				dtiParams.setbValues(bValues);
            				Preferences.debug("Just did dtiParams.setbValues(bValues)\n", Preferences.DEBUG_FILEIO);
            			} // if (bValuesOkay)
            		} // if (numFound == numVolumes)
            	} // if (okay)
            } // else if (parseString[0].equalsIgnoreCase("##$PVM_DwEffBval"))
            else if (parseString[0].equalsIgnoreCase("##$Method")) {

                if (parseString.length == 2) {
                	String method = null;
                    if (parseString[1].equalsIgnoreCase("DtiEpi")) {
                    	method = "Diffusion Tensor Imaging Echo-Planar Imaging";
                    }
                    else if (parseString[1].equalsIgnoreCase("SINGLEPULSE")) {
                    	method = "Single Pulse";
                    }
                    else if (parseString[1].equalsIgnoreCase("NSPECT")) {
                    	method = "Non-localized Spectroscopy";
                    }
                    else if (parseString[1].equalsIgnoreCase("PRESS")) {
                    	method = "Point-Resolved Spectroscopy";
                    }
                    else if (parseString[1].equalsIgnoreCase("CSI")) {
                    	method = "Chemical Shift Imaging";
                    }
                    else if (parseString[1].equalsIgnoreCase("FLASH")) {
                    	method = "Fast Low Angle Shot";
                    }
                    else if (parseString[1].equalsIgnoreCase("MSME")) {
                        method = "Multi Slice Multi Echo";	
                    }
                    else if (parseString[1].equalsIgnoreCase("RARE")) {
                    	method = "Rapid Acquisition with Relaxation Enhancement";
                    }
                    else if (parseString[1].equalsIgnoreCase("RAREVTR")) {
                    	method = "RARE with variable repetition time TR";
                    }
                    else if (parseString[1].equalsIgnoreCase("FISP")) {
                    	method = "Fast Imaging with Steady State Precession";
                    }
                    else if (parseString[1].equalsIgnoreCase("MGE")) {
                    	method = "Multiple Gradient Echo";
                    }
                    else if (parseString[1].equalsIgnoreCase("EPI")) {
                    	method = "Echo-Planar Imaging";
                    }
                    else if (parseString[1].equalsIgnoreCase("FAIR_EPI")) {
                    	method = "Flow-sensitive Alternating IR Echo-Planar Imaging";
                    }
                    else if (parseString[1].equalsIgnoreCase("DtiStandard")) {
                    	method = "Diffusion Tensor Imaging Standard";
                    }
                    else if (parseString[1].equalsIgnoreCase("SPIRAL")) {
                    	method = "SPIRAL";
                    }
                    else if (parseString[1].equalsIgnoreCase("GEFC")) {
                    	method = "Gradient Echo with Flow Compensation";
                    }
                    else if (parseString[1].equalsIgnoreCase("FL2D_ANGIO")) {
                    	method = "FL2D Angiography Method";
                    }
                    else if (parseString[1].equalsIgnoreCase("FC2D_ANGIO")) {
                    	method = "FC2D Angiography Method";
                    }
                    else if (parseString[1].equalsIgnoreCase("MDEFT")) {
                    	method = "Modified Driven-Equilibrium Fourier Transform";
                    }
                    else if (parseString[1].equalsIgnoreCase("RfProfile")) {
                    	method = "Method to measure RF Profiles";
                    }
                    else {
                    	method = parseString[1];
                    }
                    fileInfo.setMethod(method);
                    Preferences.debug("Method = " + method + "\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    raFile.close();
                    throw new IOException("##$Method has parseString with length = " + parseString.length);
                }
            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_EffSWh")) {

                if (parseString.length == 2) {

                    try {
                        double effectiveSpectralBandwidth = Double.valueOf(parseString[1]).doubleValue();
                        fileInfo.setEffectiveSpectralBandwidth(effectiveSpectralBandwidth);
                        Preferences.debug("Effective bandwidth of data sampling \n\tduring the frequency encoding period = " 
                        		          + effectiveSpectralBandwidth + "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Effective spectral bandwidth could not be read.\n", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_EffSWh has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$EchoTime")) {

                if (parseString.length == 2) {

                    try {
                        double echoTime = Double.valueOf(parseString[1]).doubleValue();;
                        fileInfo.setEchoTime(echoTime);
                        Preferences.debug("Delay between the effective centre of the excitation pulse"
                                         + "\n\t(depends on pulse rephasing properties) and "
                                         + "\n\tthe acquisition of the k-space centre = " 
                        		          + echoTime + "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Echo time could not be read.\n", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$EchoTime has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$NSegments")) {

                if (parseString.length == 2) {

                    try {
                        int numberOfSegments = Integer.valueOf(parseString[1]).intValue();;
                        fileInfo.setNumberOfSegments(numberOfSegments);
                        Preferences.debug("Number of segments in k-space = " 
                        		          + numberOfSegments + "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Number of segments could not be read.\n", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$NSegments has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_RepetitionTime")) {

                if (parseString.length == 2) {

                    try {
                        double repetitionTime = Double.valueOf(parseString[1]).doubleValue();;
                        fileInfo.setRepetitionTime(repetitionTime);
                        Preferences.debug("Delay between corresponding slices in consecutive volumes = " 
                        		        + repetitionTime + "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Delay between correspolnding slices in consecutive volumes could not be read.\n", 
                        		           Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_RepetitionTime has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PackDel")) {

                if (parseString.length == 2) {

                    try {
                        double delayBetweenVolumes = Double.valueOf(parseString[1]).doubleValue();;
                        fileInfo.setDelayBetweenVolumes(delayBetweenVolumes);
                        Preferences.debug("Delay between consecutive groups of slices (volume) "
                        		        + "\n\twhen repetitions > 1 = " 
                        		        + delayBetweenVolumes + "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Delay between volumes could not be read.", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PackDel has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_NAverages")) {

                if (parseString.length == 2) {

                    try {
                         int numberOfAverages = Integer.valueOf(parseString[1]).intValue();;
                        fileInfo.setNumberOfAverages(numberOfAverages);
                        Preferences.debug("Number of accumulations which are averaged to increase "
                        		+ "\n\tthe signal-to-noise ratio of the spectra =  " + numberOfAverages + "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Number of averages could not be read.", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_NAverages has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_NRepetitions")) {

                if (parseString.length == 2) {

                    try {
                        int numberOfRepetitions = Integer.valueOf(parseString[1]).intValue();;
                        fileInfo.setNumberOfRepetitions(numberOfRepetitions);
                        Preferences.debug("Number of repetitions of experiments = " 
                        		          + numberOfRepetitions + "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Number of repetitions could not be read.\n", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_NRepetitions has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_ScanTimeStr")) {
                if (parseString.length == 4) {
                	okay = true;
                	if (parseString[1].equals("(")) {
	                	Preferences.debug("For PVM_ScanTimeStr parseString[1] == '(' as expected\n", Preferences.DEBUG_FILEIO);
	                }
	                else
	                {
	                	okay = false;
	                	Preferences.debug("For PVM_DwBMat parseString[1] unexpectedly == " + parseString[1] + "\n", 
	                			Preferences.DEBUG_FILEIO);
	                }
                	if (okay) {
	                	try {
	                	    int stringLength = Integer.valueOf(parseString[2]).intValue();
	                	    Preferences.debug("String length in PVM_ScanTimeStr = " + stringLength + "\n", Preferences.DEBUG_FILEIO);
	                	}
	                	catch(NumberFormatException nfe) {
	                		okay = false;
	                		Preferences.debug("String length of PVM_ScanTimeStr could not be read.\n", Preferences.DEBUG_FILEIO);
	                	}
                	}
                	if (okay) {
	                	if (parseString[3].equals(")")) {
	                        Preferences.debug("For PVM_ScanTimeStr parseString[3] == ')' as expected\n", Preferences.DEBUG_FILEIO);	
	                    }
	                    else {
	                    	Preferences.debug("For PVM_ScanTimeStr parseString[3] unexpectedly == " + parseString[3] + "\n",
	                    			Preferences.DEBUG_FILEIO);
		                	okay = false;	
	                    }
                	}
                    if (okay) {
                    	lineString = readLine();
                    	index0 = lineString.indexOf('<');
                    	index1 = lineString.indexOf('>');
                    	if ((index0 >= 0) && (index1 > index0)) {
                    		String scanTime = lineString.substring(index0+1, index1);
                    		fileInfo.setScanTime(scanTime);
                    		Preferences.debug("Total duration of the experiment = " + scanTime + "\n", Preferences.DEBUG_FILEIO);
                    	}
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_ScanTimeStr has parseString with length = " + parseString.length);
                }
            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_DeriveGains")) {

                if (parseString.length == 2) {
                    String deriveGains = null;
                    if (parseString[1].equalsIgnoreCase("Yes")) {
                    	deriveGains = "The pulse gains within the sequence are derived from" +
                                      "\n\tthe global system reference gain. This assumes that" + 
                                      "\n\tthe auto-adjustment of the reference gain was performed" +
                                      "\n\tduring the same Study, i.e. by running the Traffic Light" +
                                      "\n\tor running a localized auto-reference gain adjustment.";
                    	fileInfo.setDeriveGains(deriveGains);
                    	Preferences.debug(deriveGains + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (parseString[1].equalsIgnoreCase("No")) {
                    	deriveGains = "The pulse gains within the sequence are not derived from" +
                                "\n\tthe global system reference gain.";
                    	fileInfo.setDeriveGains(deriveGains);
                    	Preferences.debug(deriveGains + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	Preferences.debug("##$PVM_DeriveGains unexpectedly has parseString[1] = " + parseString[1] + "\n",
                    			          Preferences.DEBUG_FILEIO);
                    }
                    
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_DeriveGains has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_DiffPrepMode")) {

                if (parseString.length == 2) {
                    String diffusionPreparation = null;
                    String diffusionPrepText = "\n\tThe first version of the DTI module" +
                            "\n\tprovides two preparation modes SpinEcho and StimulatedEcho. The" +
                            "\n\tSpinEcho mode sets up a slice selective refocusing 180 deg pulse" +
                            "\n\tsurrounded by the diffusion gradients. In this mode the total" + 
                            "\n\tduration of the diffusion module contributes to the minimum" + 
                            "\n\tpossible echo time (TE). The StimulatedEcho mode sets up a" +
                            "\n\tmixing period surrounded by two slice selective 90 deg. pulses." +
                            "\n\tIn this mode, the Diffusion Gradient Separation might be" +
                            "\n\tmodified without an effect to the minimum echo time, since it" +
                            "\n\tcontrols the mixing period. Only the Diffusion Gradient" + 
                            "\n\tDuration affects the minimum echo time. This mode should be" +
                            "\n\tchosen in any case where the T2 relaxation time is critical" +
                            "\n\tand/or longer diffusion times are desired. A DoubleSpinEcho" + 
                            "\n\tmode is in preparation but defaults in the first version of" +
                            "\n\tthe DTI module to a SpinEcho preparation.";
                    if (parseString[1].equalsIgnoreCase("SpinEcho")) {
                    	diffusionPreparation = "The preparation mode is spin echo." + diffusionPrepText;
                    	fileInfo.setDiffusionPreparation(diffusionPreparation);
                    	Preferences.debug(diffusionPreparation + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (parseString[1].equalsIgnoreCase("StimulatedEcho")) {
                    	diffusionPreparation = "The preparation mode is stimulated echo." + diffusionPrepText;
                    	fileInfo.setDiffusionPreparation(diffusionPreparation);
                    	Preferences.debug(diffusionPreparation + "\n", Preferences.DEBUG_FILEIO);	
                    }
                    else {
                    	Preferences.debug("##$PVM_DiffPrepMode unexpectedly has parseString[1] = " + parseString[1] + "\n",
                    			          Preferences.DEBUG_FILEIO);
                    }
                    
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_DiffPrepMode has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_DwUsedSliceThick")) {

                if (parseString.length == 2) {

                    try {
                        double usedSliceThickness = Double.valueOf(parseString[1]).doubleValue();;
                        fileInfo.setUsedSliceThickness(usedSliceThickness);
                        Preferences.debug("The slice thickness used to calculate the slice grFdients = " + usedSliceThickness + 
                                          "\n\tFor methods providing a Bandwidth Scaling factor" + 
                        		          "\n\tdifferent from 100% this value differs from PVM_SliceThick." +
                        		          "\n", Preferences.DEBUG_FILEIO);
                    } catch(NumberFormatException nfe) {
                        Preferences.debug("Used slice thickness could not be read.", Preferences.DEBUG_FILEIO);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_DwUsedSliceThick has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_DwVisiblePars")) {

                if (parseString.length == 2) {
                    String showAllParameters = null;
                    if (parseString[1].equalsIgnoreCase("Yes")) {
                    	showAllParameters = "All parameters controlled by the DTI module are" +
                                            "\n\tvisible in the method editor.";
                    	fileInfo.setShowAllParameters(showAllParameters);
                    	Preferences.debug(showAllParameters + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (parseString[1].equalsIgnoreCase("No")) {
                    	showAllParameters = "Not all parameters controlled by the DTI module are" +
                                "\n\tvisible in the method editor.  The Diffusion Output parameter" +
                    			"\n\tclass is not visible";
                    	fileInfo.setShowAllParameters(showAllParameters);
                    	Preferences.debug(showAllParameters + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	Preferences.debug("##$PVM_DwVisiblePars unexpectedly has parseString[1] = " + parseString[1] + "\n",
                    			          Preferences.DEBUG_FILEIO);
                    }
                    
                } else {
                    raFile.close();
                    throw new IOException("##$PVM_DwVisiblePars has parseString with length = " + parseString.length);
                }

            }
            else if (parseString[0].equalsIgnoreCase("##$PVM_DwRfcPulseEnum")) {

                if (parseString.length == 2) {
                	String rfcPulseType = null;
                    if ((parseString[1].equalsIgnoreCase("bp"))|| (parseString[1].equalsIgnoreCase("bp32"))) {
                    	rfcPulseType = "32 point bp pulse used for non-selective refocusing with durations above 0.2 msec";
                    }
                    else if ((parseString[1].equalsIgnoreCase("gauss"))|| (parseString[1].equalsIgnoreCase("gauss"))) {
                    	rfcPulseType = "512 point gauss pulse used for selective refocusing" +
                        "\n\tlow bandwidth factor with durations above 2 msec";
                    }
                    else if (parseString[1].equalsIgnoreCase("hermite")) {
                    	rfcPulseType = "hermite pulse with selective refocusing" + 
                        "\n\tmedium bandwidth factor";
                    }
                    else if ((parseString[1].equalsIgnoreCase("sinc"))|| (parseString[1].equalsIgnoreCase("sinc"))) {
                    	rfcPulseType = "3 lobed sinc pulse used for selective refocusing" +
                        "\n\tmedium bandwidth factor";
                    }
                    else if (parseString[1].equalsIgnoreCase("mao")) {
                    	rfcPulseType = "mao pulse with selective refocusing" +
                        "\n\thigh bandwidth factor" +
                        "\n\tGood refocusing profile";
                    }
                    else {
                    	rfcPulseType = parseString[1];
                    }
                    fileInfo.setRefocusingPulseType(rfcPulseType);
                    Preferences.debug("Refocusing pulse type = " + rfcPulseType + "\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    raFile.close();
                    throw new IOException("##$PVM_DwRFcPulseEnum has parseString with length = " + parseString.length);
                }
            }

            lineString = readLine();
        } // while (lineString != null)

        raFile.close();
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readacqp() throws IOException {
        String lineString = null;
        String[] parseString;
        String sliceSeparationMode = null;
        float sliceSeparation = -1.0f;
        float sliceThickness;
        int ni = -1;
        int nr = -1;
        int[] imageExtents;
        int xDim;
        int yDim;
        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$ACQ_slice_sepn_mode")) {

                if (parseString.length == 2) {

                    if (parseString[1].equalsIgnoreCase("Var_Angle")) {
                        sliceSeparationMode = "Variable angle";
                        fileInfo.setSliceSeparationMode("Variable angle");
                    } else if (parseString[1].equalsIgnoreCase("Var_Parallel")) {
                        sliceSeparationMode = "Variable parallel";
                        fileInfo.setSliceSeparationMode("Variable parallel");
                    } else if (parseString[1].equalsIgnoreCase("Equidistant")) {
                        sliceSeparationMode = "Equidistant";
                        fileInfo.setSliceSeparationMode("Equidistant");
                    } else if (parseString[1].equalsIgnoreCase("Contiguous")) {
                        sliceSeparationMode = "Contiguous";
                        fileInfo.setSliceSeparationMode("Contiguous");
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$ACQ_slice_sepn_mode has parseString with length = " + parseString.length);
                }

            } else if (parseString[0].equalsIgnoreCase("##$ACQ_slice_sepn")) {
                lineString = readLine();
                parseString = parse(lineString);
                sliceSeparation = Float.valueOf(parseString[0]).floatValue();
            } else if (parseString[0].equalsIgnoreCase("##$ACQ_slice_thick")) {

                if (parseString.length == 2) {
                    sliceThickness = Float.valueOf(parseString[1]).floatValue();
                    fileInfo.setSliceThickness(sliceThickness);
                } else {
                    raFile.close();
                    throw new IOException("##$ACQ_slice_thick has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$NI")) {
            	if (parseString.length == 2) {
                    ni = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$NI has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$NR")) {
            	if (parseString.length == 2) {
                    nr = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$NR has parseString with length = " + parseString.length);
                }	
            }

            lineString = readLine();
        } // while (lineString != null)

        raFile.close();

        if ((sliceSeparation > 0.0f) && (fileInfo.getRecoSize() == 2) &&
                ((sliceSeparationMode == "Equidistant") || (sliceSeparationMode == "Contiguous"))) {
            imgResols = fileInfo.getResolutions();
            imgResols[2] = sliceSeparation;
            fileInfo.setResolutions(imgResols);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            fileInfo.setHaveZResol(true);
        }
        
        if ((ni > 1) && (nr > 1)) {
        	imageExtents = fileInfo.getExtents();
        	if ((imageExtents.length == 3) && ((ni * nr) == imageExtents[2])) {
        	    xDim = imageExtents[0];
        	    yDim = imageExtents[1];
        	    imageExtents = new int[4];
        	    imageExtents[0] = xDim;
        	    imageExtents[1] = yDim;
        	    imageExtents[2] = ni;
        	    imageExtents[3] = nr;
        	    fileInfo.setExtents(imageExtents);
        	}
        }

    }


    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readd3proc() throws IOException {

        // This reads d3proc header file of the BRUKER file format
        String lineString = null;
        String[] parseString;
        int dataType;
        int xDim = 0;
        int yDim = 0;
        int zDim = 0;
        int tDim = 0;
        int[] imageExtents;

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        fileInfo = new FileInfoBRUKER(fileName, fileDir, FileUtility.BRUKER); // dummy fileInfo
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$DATTYPE")) {

                if (parseString.length == 2) {

                    if (parseString[1].equals("ip_byte")) { // Educated guess
                        fileInfo.setDataType(ModelImage.BYTE);
                    } else if (parseString[1].equals("ip_short")) { // From actual data.
                        fileInfo.setDataType(ModelImage.SHORT);
                    } else if ((parseString[1].equals("ip_integer")) ||
                    		   (parseString[1].equals("ip_int"))) { // Educated guess
                        fileInfo.setDataType(ModelImage.INTEGER);
                    } else {
                        dataType = Integer.valueOf(parseString[1]).intValue();

                        if (dataType == 2) {
                            fileInfo.setDataType(ModelImage.UBYTE);
                        } else if (dataType == 3) {
                            fileInfo.setDataType(ModelImage.SHORT);
                        } else if (dataType == 5) {
                            fileInfo.setDataType(ModelImage.INTEGER);
                        }
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$DATTYPE has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIX")) {

                if (parseString.length == 2) {
                    xDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIX has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIY")) {

                if (parseString.length == 2) {
                    yDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIY has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIZ")) {

                if (parseString.length == 2) {
                    zDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIZ has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$IM_SIT")) {

                if (parseString.length == 2) {
                    tDim = Integer.valueOf(parseString[1]).intValue();
                } else {
                    raFile.close();
                    throw new IOException("##$IM_SIT has parseString with length = " + parseString.length);
                }
            }

            lineString = readLine();
        } // while (lineString != null)

        raFile.close();

        if ((xDim > 1) && (yDim > 1) && (zDim > 1) && (tDim > 1)) {
            imageExtents = new int[4];
            imageExtents[0] = xDim;
            imageExtents[1] = yDim;
            imageExtents[2] = zDim;
            imageExtents[3] = tDim;
            fileInfo.setExtents(imageExtents);
        } else if ((xDim > 1) && (yDim > 1) && (zDim > 1)) {
            imageExtents = new int[3];
            imageExtents[0] = xDim;
            imageExtents[1] = yDim;
            imageExtents[2] = zDim;
            fileInfo.setExtents(imageExtents);
        } else if ((xDim > 1) && (yDim > 1)) {
            imageExtents = new int[2];
            imageExtents[0] = xDim;
            imageExtents[1] = yDim;
            fileInfo.setExtents(imageExtents);
        }

    }

    /**
     * This method reads one slice from the file into byteBuffer and then transfers into the float array imgBuffer.
     *
     * @param      one  DOCUMENT ME!
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean one) throws IOException {
        int i;
        int[] imageExtents;
        float[] imgBuffer = null;
        int length;
        int middleSlice = 0;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            imageExtents = fileInfo.getExtents();
            length = imageExtents[0] * imageExtents[1];

            if (imageExtents.length == 4) {
                imageSlice = imageExtents[2] * imageExtents[3];
            } else if (imageExtents.length == 3) {
                imageSlice = imageExtents[2];
            } else {
                imageSlice = 1;
            }

            if (one) {
                imageSlice = 1;
                imageExtents = new int[2];
                imageExtents[0] = fileInfo.getExtents()[0];
                imageExtents[1] = fileInfo.getExtents()[1];

                if (imageExtents.length > 3) {
                    middleSlice = imageExtents[2] / 2;
                }
            }

            imgBuffer = new float[length];

            image = new ModelImage(fileInfo.getDataType(), imageExtents, fileName);
            if(prefImageName != null) {
                image.setImageName(prefImageName);
            }

            for (i = 0; i < imageSlice; i++) {

                try {

                    if (one) {
                        readBuffer(middleSlice, imgBuffer);
                    } else {
                        readBuffer(i, imgBuffer); // Slice a time
                    }

                    fileInfoCopy = (FileInfoBRUKER)fileInfo.clone();
                    image.setFileInfo(fileInfoCopy, i);
                } catch (IOException error) {
                    throw new IOException("File BRUKER: read: " + error);
                }

                image.importData(i * length, imgBuffer, false);
            }

            raFile.close();

        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            imgBuffer = null;
            System.gc();
            throw error;
        }

        return image;

    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void readreco() throws IOException {

        // This reads the reco file of the BRUKER file format
        String lineString = null;
        String[] parseString;

        float[] fov = null;
        int[] recoSize = null;
        int i;
        float temp;

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "r");
        lineString = readLine();

        while (lineString != null) {
            parseString = parse(lineString);

            if (parseString[0].equalsIgnoreCase("##$RECO_wordtype")) {

                if (parseString.length == 2) {

                    if (parseString[1].equalsIgnoreCase("_8BIT_UNSGN_INT")) {
                        fileInfo.setDataType(ModelImage.UBYTE);
                    } else if (parseString[1].equalsIgnoreCase("_16BIT_SGN_INT")) {
                        fileInfo.setDataType(ModelImage.SHORT);
                    } else if (parseString[1].equalsIgnoreCase("_32_BIT_SGN_INT")) {
                        fileInfo.setDataType(ModelImage.INTEGER);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$RECO_wordtype has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$RECO_byte_order")) {

                if (parseString.length == 2) {

                    if (parseString[1].equalsIgnoreCase("bigEndian")) {
                        fileInfo.setEndianess(FileBase.BIG_ENDIAN);
                    } else if (parseString[1].equalsIgnoreCase("littleEndian")) {
                        fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
                    }
                } else {
                    raFile.close();
                    throw new IOException("##$RECO_wordtype has parseString with length = " + parseString.length);
                }
            } else if (parseString[0].equalsIgnoreCase("##$RECO_size")) {
                lineString = readLine();
                parseString = parse(lineString);
                fileInfo.setRecoSize(parseString.length);
                
                recoSize = new int[parseString.length];
                for (i = 0; i < parseString.length; i++) {
                	recoSize[i] = Integer.valueOf(parseString[i]).intValue();
                }
            } else if (parseString[0].equalsIgnoreCase("##$RECO_fov")) {
                lineString = readLine();
                parseString = parse(lineString);
                fov = new float[parseString.length];

                for (i = 0; i < parseString.length; i++) {
                    fov[i] = Float.valueOf(parseString[i]).floatValue();
                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), i);
                }


                if (parseString.length > 2) {
                    fileInfo.setHaveZResol(true);
                }
            }

            lineString = readLine();
        } // while (lineString != null)
        
        if (fov != null) {
        	if ((recoSize != null) && (fov.length >= 2) && (recoSize.length >= 2) && (recoSize[0] != recoSize[1]) &&
        	    (recoSize[0] == fileInfo.getExtents()[1]) && (recoSize[1] == fileInfo.getExtents()[0])) {
        		  temp = fov[0];
        		  fov[0] = fov[1];
        		  fov[1] = temp;
        	}
        	for (i = 0; i < fov.length; i++) {
        		imgResols[i] = fov[i] * 10.0f / fileInfo.getExtents()[i];
        	}
        	fileInfo.setResolutions(imgResols);
        } // if (fov != null)

        raFile.close();
    }

    /**
     * Accessor to set the file directoyr (used for reading BRUKER files).
     *
     * @param  fDir  file directory of image to read.
     */
    public void setFileDir(String fDir) {
        fileDir = fDir;
    }

    /**
     * Accessor to set the file name (used for reading BRUKER files).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   inString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String[] parse(String inString) {
        String[] tmpString = new String[50];
        String[] outString;
        int i;
        int sNum = 0;
        int firstEl = 0;

        for (i = 0; i < inString.length(); i++) {

            if ((inString.charAt(i) <= 0x20) || (inString.charAt(i) == '=')) {

                if (firstEl != i) {
                    tmpString[sNum++] = inString.substring(firstEl, i);
                }

                firstEl = i + 1;
            }
        }

        if (firstEl != i) {
            tmpString[sNum++] = inString.substring(firstEl, i);
        }

        if (sNum == 0) {
            outString = new String[1];
            outString[0] = inString;
        } else {
            outString = new String[sNum];

            for (i = 0; i < (sNum); i++) {
                outString[i] = tmpString[i];
            }
        }

        return outString;

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readBuffer(int slice, float[] buffer) throws IOException {
        int i, j;
        int b1, b2, b3, b4;
        long progress, progressLength, mod;

        // long pointer;
        int nBytes;
        byte[] byteBuffer;

        i = 0;

        try {

            switch (fileInfo.getDataType()) {

                case ModelStorageBase.UBYTE:
                    nBytes = buffer.length;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * imageSlice;
                    mod = progressLength / 10;

                    for (j = 0; j < nBytes; j++, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        buffer[i] = byteBuffer[j] & 0xff;
                    }
                    break;

                case ModelStorageBase.SHORT:
                    nBytes = 2 * buffer.length;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * imageSlice;
                    mod = progressLength / 10;

                    for (j = 0; j < nBytes; j += 2, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        b1 = getUnsignedByte(byteBuffer, j);
                        b2 = getUnsignedByte(byteBuffer, j + 1);

                        if (fileInfo.getEndianess()) {
                            buffer[i] = (short) ((b1 << 8) + b2);
                        } else {
                            buffer[i] = (short) ((b2 << 8) + b1);
                        }

                    }

                    break;

                case ModelStorageBase.INTEGER:
                    nBytes = 4 * buffer.length;
                    byteBuffer = new byte[nBytes];
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * imageSlice;
                    mod = progressLength / 10;

                    for (j = 0; j < nBytes; j += 4, i++) {

                        if (((i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                        }

                        b1 = getUnsignedByte(byteBuffer, j);
                        b2 = getUnsignedByte(byteBuffer, j + 1);
                        b3 = getUnsignedByte(byteBuffer, j + 2);
                        b4 = getUnsignedByte(byteBuffer, j + 3);

                        if (fileInfo.getEndianess()) {
                            buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                        } else {
                            buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                        }
                    }

                    break;
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

    }

    /**
     * Reads lines of the file until a nonnull String results or the end of the file is reached.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString = null;

        while ((tempString == null) && (raFile.getFilePointer() < (raFile.length() - 1)) && (!foundEOF)) {

            try {
                tempString = raFile.readLine();
            } catch (EOFException error) {
                tempString = null;
                foundEOF = true;
            } catch (IOException error) {
                throw (error);
            }


            if (tempString != null) {

                if (tempString.length() == 0) {
                    tempString = null;
                }
            }
        } // while

        return tempString;
    }

    /**
     * This convenience method is needed to establish the location of the 2dseq file
     */
    public String getFileDir() {
        return fileDir;
    }

}
