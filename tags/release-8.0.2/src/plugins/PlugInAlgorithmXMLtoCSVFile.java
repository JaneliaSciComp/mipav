
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmXMLtoCSVFile extends AlgorithmBase {

	private File XMLDirectoryFile;
	
	private String CSVFileName;
	
	private JTextArea outputTextArea;
	
	private RandomAccessFile raFile = null;
	
	private String name;
	
	String outputName;
	
	private RandomAccessFile raInputFile = null;


	public PlugInAlgorithmXMLtoCSVFile(File XMLDirectoryFile, String CSVFileName, JTextArea outputTextArea) {
		this.XMLDirectoryFile = XMLDirectoryFile;
		this.CSVFileName = CSVFileName;
		this.outputTextArea = outputTextArea;
		
	}
	
	
	public void runAlgorithm() {
		File CSVFile;
		String currDir;
		String line;
		int i;
		int j;
		File files[];
		boolean firstNumFound;
		Character ch;
		int beginIndex = 0;
		int endIndex = 0;
		String id;
		int valuesRead;
		int index;
		String sval;
		String value;
		
		outputTextArea.append("Running Algorithm" + "\n");
        
        final long begTime = System.currentTimeMillis();
        currDir = XMLDirectoryFile.getAbsolutePath();
        CSVFile = new File(currDir + File.separator + CSVFileName);
        outputName = CSVFile.getName();
        try {
            raFile = new RandomAccessFile(CSVFile, "rw");
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
        	close();
        	setCompleted(false);
        	return;
        }
        
        try {
        	raFile.writeBytes("ID,DICE,JACRD,AUC,KAPPA,RNDIND,ADJRIND,ICCORR,VOLSMTY,MUTINF,HDRFDST,AVGDIST,"+
                              "MAHLNBS,VARINFO,GCOERR,PROBDST,SNSVTY,SPCFTY,PRCISON,FMEASR,ACURCY,FALLOUT\n");
        }
        catch (IOException e) {
    		MipavUtil.displayError("IOException " + e + " raFile.writeBytes");
    		close();
    		setCompleted(false);
    		return;
    	}		
        
        files = XMLDirectoryFile.listFiles();
        Arrays.sort(files, new fileComparator());
        for (i = 0; i < files.length; i++) {
        	if ((files[i].isFile()) && (files[i].getName().toLowerCase().endsWith(".xml"))) {
        	    name = files[i].getName();
        	    firstNumFound = false;
        	    for (j = 0; j < name.length(); j++) {
        	        ch = name.charAt(j);
        	        if (!firstNumFound && Character.isDigit(ch)) {
        	        	firstNumFound = true;
        	        	beginIndex = j;
        	        }
        	        else if (firstNumFound && !Character.isDigit(ch)) {
        	        	endIndex = j;
        	        	break;
        	        }
        	    } // for (j = 0; j < name.length(); j++)
        	    id = name.substring(beginIndex, endIndex);
        	    try {
                    raInputFile = new RandomAccessFile(files[i], "r");
                } catch (final FileNotFoundException e) {
                    MipavUtil.displayError("raInputFile FileNotFoundException " + e);
                    close();
                    setCompleted(false);
                    return;
                }
        	    try {
        	        raFile.writeBytes(id + ",");
        	    }
        	    catch (IOException e) {
            		MipavUtil.displayError("IOException " + e + " raFile.writeBytes");
            		close();
            		closeInput();
            		setCompleted(false);
            		return;
            	}	
        	    valuesRead = 0;
        	    while (valuesRead < 21) {
        	    	try {
                    	line = raInputFile.readLine();
                    }
                    catch (IOException e) {
                    	MipavUtil.displayError("raInputFile.readLine() gives IOException " + e + " on " + name);
                        close();
                        closeInput();
                        setCompleted(false);
                        return;
                    }
                    
                    if ((line == null) || (line.length() == 0)) {
                    	continue;
                    }
                    
                    index = line.indexOf("value=");
                    if (index == -1) {
                    	continue;
                    }
                    firstNumFound = false;
                    for (j = index+6; j < line.length(); j++) {
                    	ch = line.charAt(j);
                    	sval = line.substring(j, j+1);
                    	if (!firstNumFound && (Character.isDigit(ch) || sval.equals("+") || sval.equals("-") || sval.equals("."))) {
                    		firstNumFound = true;
                    		beginIndex = j;
                    	}
                    	else if (firstNumFound && (!Character.isDigit(ch)) && (!sval.equals("+")) && (!sval.equals("-")) &&
                    			             (!sval.equals("."))) {
                    	    endIndex = j;
                    	    break;
                    	}
                    } // for (j = index+6; j < line.length(); j++)
                    value = line.substring(beginIndex, endIndex);
                    valuesRead++;
                    if (valuesRead < 21) {
                    	try {
                	        raFile.writeBytes(value + ",");
                	    }
                	    catch (IOException e) {
                    		MipavUtil.displayError("IOException " + e + " raFile.writeBytes");
                    		close();
                    		closeInput();
                    		setCompleted(false);
                    		return;
                    	}		
                    } // if (valuesRead < 21)
                    else {
                    	try {
                	        raFile.writeBytes(value + "\n");
                	    }
                	    catch (IOException e) {
                    		MipavUtil.displayError("IOException " + e + " raFile.writeBytes");
                    		close();
                    		closeInput();
                    		setCompleted(false);
                    		return;
                    	}			
                    }
        	    } // while (valuesRead < 21)
        	    
        	    closeInput();
        	} // if ( (files[i].isFile()) && (files[i].getName().toLowerCase().endsWith(".xml")))
        } // for (i = 0; i < files.length; i++)
        
        close();
        outputTextArea.append(outputName + " written to disk\n");
		
		final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

	}
	
	private void close() {
		try {
            raFile.close();
        }
        catch (IOException e) {
        	MipavUtil.displayError("raFile.close gives IOException " + e + " on " + outputName);
            setCompleted(false);
            return;		
        }	
	}
	
	private void closeInput() {
		try {
            raInputFile.close();
        }
        catch (IOException e) {
        	MipavUtil.displayError("raInputFile.close gives IOException " + e + " on " + name);
        	close();
            setCompleted(false);
            return;		
        }	
	}
	
	private class fileComparator implements Comparator<File> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        @Override
        public int compare(final File o1, final File o2) {
            final String a = o1.getName();
            final String b = o2.getName();
            return a.compareToIgnoreCase(b);
        }

    }
	

}
