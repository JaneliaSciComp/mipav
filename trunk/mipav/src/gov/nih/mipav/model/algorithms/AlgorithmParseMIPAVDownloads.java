package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;
import java.io.*;

import java.util.*;



public class AlgorithmParseMIPAVDownloads extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------


    /** DOCUMENT ME! */
    private String dirPath; // directory to recursively operate in

    /** DOCUMENT ME! */
    private int numDownloads = 0; // number of old xml files that were converted

    private int numUsers = 0;
    
    private int nihDownloads = 0;
    
    private int nihUsers = 0;
    
    private int outsideNIHDownloads = 0;
    
    private int outsideNIHUsers = 0;
    
    
    private Hashtable<String, String> emailTable = new Hashtable<String, String>();
    
    private Hashtable<String, Integer> nihTable = new Hashtable<String, Integer>();
    
    private Hashtable<String, Integer> nihDownloadTable = new Hashtable<String, Integer>();
    
    private Hashtable<String, Integer> outsideInstTable = new Hashtable<String, Integer>();
    
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default Constructor.
     *
     * @param  dir  full pathname of directory to traverse
     */
    public AlgorithmParseMIPAVDownloads(String dir) {
        this.dirPath = dir;

        // clip off the trailing file separator
        if (dirPath.endsWith(File.separator)) {
            dirPath = dirPath.substring(0, dirPath.length() - 1);
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        Vector<String> fileVector = new Vector<String>();

        System.err.println("Top directory is: " + dirPath);
        addFilesToVector(dirPath, fileVector);

        int size = fileVector.size();
        for (int i = 0; i < size; i++) {
            parseFile(fileVector.elementAt(i));
        }
        
        String appMessage = new String();
       
        appMessage = "MIPAV download parse: " + "\n";
        
        appMessage += "\tTotal downloads: " + numDownloads + "\n";
        appMessage += "\tNIH downloads: " + nihDownloads + "\n";
        
        
        appMessage += "\tNIH downloads by institution: " + "\n";
        Enumeration<String> e = nihDownloadTable.keys();
        String inst;
        while (e.hasMoreElements()) {
        	inst = e.nextElement();
        	appMessage += "\t\t" + inst + ": " + nihDownloadTable.get(inst) + "\n";
        }
        
        appMessage += "\tOutside downloads: " + outsideNIHDownloads + "\n\n";
        
        appMessage += "\tTotal users: " + numUsers + "\n";
        
        appMessage += "\tNIH users: " + nihUsers + "\n";
        appMessage += "\tNIH users by institution: " + "\n";
        e = nihTable.keys();
        while (e.hasMoreElements()) {
        	inst = e.nextElement();
        	appMessage += "\t\t" + inst + ": " + nihTable.get(inst) + "\n";
        }
                
        appMessage += "\tOutside users: " + outsideNIHUsers + "\n";
        
        appMessage += "\tOutside users by institution type: " + "\n";
        e = outsideInstTable.keys();
        while (e.hasMoreElements()) {
        	inst = e.nextElement();
        	appMessage += "\t\t" + inst + ": " + outsideInstTable.get(inst) + "\n";
        }
        
        
        ViewUserInterface.getReference().getMessageFrame().append(appMessage, ViewJFrameMessage.DATA);

    }

    /**
     * Recursively adds file and directory paths to a Vector.
     *
     * @param  name  The name of either file or directory
     * @param  vec   Vector that holds all files to be processed
     */
    private void addFilesToVector(String name, Vector<String> vec) {
        File tempFile = new File(name);

        if (tempFile.isDirectory()) {
            String[] fileList = tempFile.list();

            for (int i = 0; i < fileList.length; i++) {
                addFilesToVector(name + File.separator + fileList[i], vec);
            }
            // vec.add(0, name);
        } else {
            vec.add(name);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fileName  DOCUMENT ME!
     */
    private void parseFile(String fileName) {

        File xmlFile = new File(fileName);

        BufferedReader bReader = null;
        String inString = null;
            
        Integer oneCount = new Integer(1);
        boolean newUser = false;
        
        //boolean determining if nih affil tag OR institution type was found (if not
        boolean isNIHorOutside = false;
        
        String lastEmail = null;
        
        try {
            bReader = new BufferedReader(new FileReader(xmlFile));

            inString = bReader.readLine();
            while (inString != null) {
            	
            	if (inString.startsWith("Name")) {
            		if (numDownloads>0) {
            			if (!isNIHorOutside) {
            				if (lastEmail.contains("nih.gov")) {
            					nihDownloads++;
            					if (newUser) {
            						nihUsers++;
            					}
            				} else {
            					outsideNIHDownloads++;
            					if (newUser) {
            						outsideNIHUsers++;
            					}
            				}
            			}
            		}
            		
            		//initialize newUser and isNIHorOutside to false
            		isNIHorOutside = false;
            		newUser = false;
            		numDownloads++;
            	} else if (inString.startsWith("email:")) {
            		inString = inString.substring(6).trim().toLowerCase();
            		lastEmail = inString.toLowerCase();
            		if (!emailTable.containsKey(inString)) {
            			newUser = true;
            			emailTable.put(inString, inString);
            			numUsers++;
            		}
            	} else if (inString.startsWith("institution type:")) {
            		outsideNIHDownloads++;
            		isNIHorOutside = true;
            		if (newUser) {
            			outsideNIHUsers++;
            			inString = inString.substring(17).trim().toLowerCase();
            			if (!outsideInstTable.containsKey(inString)) {
            				outsideInstTable.put(inString, new Integer(1));
            			} else {
            				Integer count = outsideInstTable.get(inString);
            				count+= oneCount;
            				outsideInstTable.remove(inString);
            				outsideInstTable.put(inString, count);
            			}
            		}
            	} else if (inString.startsWith("nih affil:")) {
            		nihDownloads++;
            		inString = inString.substring(10).trim();
            		isNIHorOutside = true;
            		
            		if (!nihDownloadTable.containsKey(inString)) {
            			nihDownloadTable.put(inString, new Integer(1));
        			} else {
        				Integer count = nihDownloadTable.get(inString);
        				count+= oneCount;
        				nihDownloadTable.remove(inString);
        				nihDownloadTable.put(inString, count);
        			}
            		
            		if (newUser) {
            			nihUsers++;
            			
            			
            			if (!nihTable.containsKey(inString)) {
            				nihTable.put(inString, new Integer(1));
            			} else {
            				Integer count = nihTable.get(inString);
            				count+= oneCount;
            				nihTable.remove(inString);
            				nihTable.put(inString, count);
            			}
            			
            		}
            	}
            	
            	inString = bReader.readLine();
            }

            //do this one last time for possible old-style end
            if (!isNIHorOutside) {
				if (lastEmail.contains("nih.gov")) {
					nihDownloads++;
					if (newUser) {
						nihUsers++;
					}
				} else {
					outsideNIHDownloads++;
					if (newUser) {
						outsideNIHUsers++;
					}
				}
			}
            
            bReader.close();
            bReader = null;

        } catch (Exception e) {
            return;
            // blah
        }

    }

}
