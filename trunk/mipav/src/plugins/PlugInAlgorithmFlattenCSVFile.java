import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Stack;

import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.util.MipavMath;


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
		outputTextArea.append("Running Algorithm v1.0" + "\n");
        
   

        final long begTime = System.currentTimeMillis();
		
		final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

	}
	
	
	

}
