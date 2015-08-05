
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;

import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmFlattenCSVFile extends AlgorithmBase {

	private File inputFile;
	
	private File outputFile;
	
	private JTextArea outputTextArea;


	public PlugInAlgorithmFlattenCSVFile(File inputFile, File outputFile, JTextArea outputTextArea) {
		this.inputFile = inputFile;
		this.outputFile = outputFile;
		this.outputTextArea = outputTextArea;
		
	}
	
	
	public void runAlgorithm() {
		String inputName;
		RandomAccessFile raFile = null;
		// Each record is a line starting with an x
		String line;
		ArrayList<String>lines = new ArrayList<String>();
		ArrayList<Integer>recordStart = new ArrayList<Integer>();
		boolean badRead = false;
		int lineNum = 0;
		int recordNum = 0;
		String fields[][];
		int i;
		int fieldNum;
		int commaNum;
		String labels[];
		int firstCommaIndex;
		String field;
		int j;
		int commaIndex;
		int nextCommaIndex;
		int maxFieldNum[];
		int k;
		int m;
		int presentFieldNum[];
		int flatFieldNum;
		String flatFields[][];
		int flatFieldStart[];
		String outputName;
		boolean badWrite = false;
		boolean badSetLength = false;
		outputTextArea.append("Running Algorithm v1.0" + "\n");
        
        final long begTime = System.currentTimeMillis();
        inputName = inputFile.getName();
        
        try {
            raFile = new RandomAccessFile(inputFile, "r");
        }
        catch (FileNotFoundException e) {
        	MipavUtil.displayError("File not found exception " + e + " on " + inputName);
            setCompleted(false);
            return;	
        }
        
        while (true) {
            try {
            	line = raFile.readLine();
            }
            catch (IOException e) {
            	MipavUtil.displayError("raFile.readLine() gives IOException " + e + " on " + inputName);
                badRead = true;
                break;
            }
            
            if (line == null) {
            	break;
            }
            
            lines.add(line);
            lineNum++;
            
            if (line.substring(0,1).equals("x")) {
            	recordStart.add(lineNum-1);
            	recordNum++;
            }
            
        } // while (true)
        
        try {
            raFile.close();
        }
        catch (IOException e) {
        	MipavUtil.displayError("raFile.close gives IOException " + e + " on " + inputName);
            setCompleted(false);
            return;		
        }
        
        if (badRead) {
        	setCompleted(false);
        	return;
        }
        
        outputTextArea.append(lineNum + " lines read\n");
        outputTextArea.append(recordNum + " records read\n");
        labels = lines.get(1).split(",");
        fieldNum = labels.length;
        outputTextArea.append(fieldNum + " fields in original file\n");
        commaNum = fieldNum-1;
        fields = new String[lineNum][fieldNum];
        for (i = 0; i < lineNum; i++) {
        	firstCommaIndex = lines.get(i).indexOf(",");
        	if (firstCommaIndex != 0) {
        	    field = lines.get(i).substring(0,firstCommaIndex).trim();
        	    if (field.length() > 0) {
        	        fields[i][0] = field;	
        	    }
        	}
        	commaIndex = firstCommaIndex;
        	for (j = 0; j < commaNum-1; j++) {
        	    nextCommaIndex = lines.get(i).indexOf(",",commaIndex+1);
        	    if (nextCommaIndex > commaIndex + 1) {
        	    	field = lines.get(i).substring(commaIndex+1,nextCommaIndex).trim();
        	    	if (field.length() > 0) {
        	    	    fields[i][j+1] = field;	
        	    	}
        	    } // if (nextCommaIndex > commaIndex + 1)
        	    commaIndex = nextCommaIndex;
        	} // for (j = 0; j < commaNum; j++)
        	if (lines.get(i).length() > commaIndex + 1) {
        		field = lines.get(i).substring(commaIndex+1).trim();
        		if (field.length() > 0) {
        			fields[i][commaNum] = field; 
        		}
        	}
        } // for (i = 0; i < lineNum; i++)
        
        lines.clear();
        
        maxFieldNum = new int[fieldNum];
        for (k = 0; k < fieldNum; k++) {
        	maxFieldNum[k] = 1;
        }
        presentFieldNum = new int[fieldNum];
        
        for (i = 0; i < recordNum-1; i++) {
        	if ((recordStart.get(i+1) - recordStart.get(i)) > 1) {
        		for (k = 0; k < fieldNum; k++) {
        			presentFieldNum[k] = 0;
        		}
        	    for (j = recordStart.get(i); j < recordStart.get(i+1); j++) {
        	        for (k = 0; k < fieldNum; k++) {
        	        	if (fields[j][k] != null) {
        	        	    presentFieldNum[k]++;	
        	        	}
        	        }
        	    } // for (j = recordStart.get(i); j < recordStart.get(i+1); j++) 
        	    for (k = 0; k < fieldNum; k++) {
        	    	if (presentFieldNum[k] > maxFieldNum[k]) {
        	    		maxFieldNum[k] = presentFieldNum[k];
        	    	}
        	    }
        	} // if ((recordStart.get(i+1) - recordStart.get(i)) > 1) 
        } // for (i = 0; i < recordNum-1; i++)
        
        if ((lineNum-1 - recordStart.get(recordNum-1)) > 1) {
        	for (k = 0; k < fieldNum; k++) {
    			presentFieldNum[k] = 0;
    		}
    	    for (j = recordStart.get(recordNum-1); j < lineNum; j++) {
    	        for (k = 0; k < fieldNum; k++) {
    	        	if (fields[j][k] != null) {
    	        	    presentFieldNum[k]++;	
    	        	}
    	        }
    	    } // for (j = recordStart.get(i); j < recordStart.get(i+1); j++) 
    	    for (k = 0; k < fieldNum; k++) {
    	    	if (presentFieldNum[k] > maxFieldNum[k]) {
    	    		maxFieldNum[k] = presentFieldNum[k];
    	    	}
    	    }	
        } // if ((lineNum-1 - recordStart.get(recordNum-1)) > 1)
        
        flatFieldNum = 0;
        for (k = 0; k < fieldNum; k++) {
        	flatFieldNum += maxFieldNum[k];
        }
        outputTextArea.append(flatFieldNum + " fields in the flat csv file\n");
        flatFields = new String[recordNum+2][flatFieldNum];
        flatFieldStart = new int[fieldNum];
        flatFields[0][0] = fields[0][0];
        flatFields[0][1] = fields[0][1];
        i = 0;
        for (k = 0; k < fieldNum; k++) {
        	flatFieldStart[k] = i;
            if (maxFieldNum[k] == 1) {
            	flatFields[1][i++] = fields[1][k];
            }
            else {
            	for (m = 1; m <= maxFieldNum[k]; m++) {
            		flatFields[1][i++] = fields[1][k] + "_" + String.valueOf(m);
            	}
            }
        } // for (k = 0; k < fieldNum; k++)
        
        for (i = 0; i < recordNum-1; i++) {
        	for (k = 0; k < fieldNum; k++) 	{
        		m = 0;
                for (j = recordStart.get(i); j < recordStart.get(i+1); j++) {
                	if (fields[j][k] != null) {
                        flatFields[i+2][flatFieldStart[k]+m] = fields[j][k];
                        m++;
                	}
                } // for (j = recordStart.get(i); j < recordStart.get(i+1); j++)
        	} // for (k = 0; k < fieldNum; k++)
        } // for (i = 0; i < recordNum-1; i++)
        
        for (k = 0; k < fieldNum; k++) {
            m = 0;
            for (j = recordStart.get(recordNum-1); j < lineNum; j++) {
            	if (fields[j][k] != null) {
                    flatFields[i+2][flatFieldStart[k]+m] = fields[j][k];
                    m++;
            	}	
            } // for (j = recordStart.get(recordNum-1); j < lineNum; j++)
        } // for (k = 0; k < fieldNum; k++)
        
        outputName = outputFile.getName();
        
        try {
            raFile = new RandomAccessFile(outputFile, "rw");
        }
        catch (FileNotFoundException e) {
        	MipavUtil.displayError("File not found exception " + e + " on " + outputName);
            setCompleted(false);
            return;	
        }
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        try {
            raFile.setLength(0);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on raFile.setLength(0)");
        	badSetLength = true;
        }
        
        loop: for (i = 0; i < recordNum+2 && (!badSetLength); i++) {
        	for (k = 0; k < flatFieldNum - 1; k++) {
        	    if (flatFields[i][k] == null) {
        	    	try {
        	    	    raFile.writeBytes(",");
        	    	}
        	    	catch (IOException e) {
        	    		MipavUtil.displayError("IOException " + e + " on raFile.writeBytes");
        	    		badWrite = false;
        	    		break loop;
        	    	}	
        	    }
        	    else {
        	        try {
        	        	raFile.writeBytes(flatFields[i][k] + ",");
        	        }
        	        catch (IOException e) {
        	    		MipavUtil.displayError("IOException " + e + " raFile.writeBytes");
        	    		badWrite = false;
        	    		break loop;
        	    	}	
        	    }
        	} // for (k = 0; k < flatFieldNum - 1; k++)
        	if (flatFields[i][flatFieldNum-1] == null) {
        		try {
    	    	    raFile.writeBytes("\n");
    	    	}
    	    	catch (IOException e) {
    	    		MipavUtil.displayError("IOException " + e + " raFile.writeBytes");
    	    		badWrite = false;
    	    		break loop;
    	    	}		
        	}
        	else {
        		try {
    	        	raFile.writeBytes(flatFields[i][flatFieldNum-1] + "\n");
    	        }
    	        catch (IOException e) {
    	    		MipavUtil.displayError("IOException " + e + " raFile.writeBytes");
    	    		badWrite = false;
    	    		break loop;
    	    	}		
        	}
        } // loop: for (i = 0; i < recordNum+2; i++)
        
        try {
            raFile.close();
        }
        catch (IOException e) {
        	MipavUtil.displayError("raFile.close gives IOException " + e + " on " + outputName);
            setCompleted(false);
            return;		
        }
        
        if (badSetLength || badWrite) {
            setCompleted(false);
            return;
        }
        
        outputTextArea.append(outputName + " written to disk\n");
		
		final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

	}
	
	
	

}
