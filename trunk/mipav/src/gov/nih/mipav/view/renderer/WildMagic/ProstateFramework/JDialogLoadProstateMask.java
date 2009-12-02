package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;



public class JDialogLoadProstateMask extends JDialogBase 
 {

	/** DOCUMENT ME! */
    private ModelImage image; // source image
    /** The main user interface. */
    private ViewUserInterface UI;

	public JDialogLoadProstateMask(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
        image = im;
        init();
	}

	 public void actionPerformed(ActionEvent event) {
		 
	 }
	
	public void init() {
		String fileName;
        String directory;
        JFileChooser chooser = new JFileChooser();

        chooser.setDialogTitle("Open Prostate Mask");

        if (UI.getDefaultDirectory() != null) {
            File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
          
            readMaskfile(fileName, directory);
        } else {
            return;
        }      
	}
	
	public void readMaskfile(String fileName, String directory) {
		try {
			FileReader fileReader = new FileReader(directory
					+ File.separatorChar + fileName);
			BufferedReader inputStream = new BufferedReader(fileReader);
			
			String line = null;
			float value;
			Vector mask = new Vector();
			while ( (line = inputStream.readLine()) != null ) {
			    
				value = Float.valueOf(line);
				
				mask.add(value);
				// System.err.println("value = " + value);
			}
			
			int xDim = image.getExtents()[0];
	        int yDim = image.getExtents()[1];
	        int sliceSize = xDim * yDim;
	        double[] sourceBuffer = new double[sliceSize];
	        
	        image.exportData(0, sliceSize, sourceBuffer);
	        
			for ( int i = 0; i < sourceBuffer.length; i++ ) {
				value = (Float)mask.get(i);
				if ( value == 1 ) {
 					sourceBuffer[i] = 5000.0;
				}
			}
			
			image.importData(0, sourceBuffer, true);
	        image.calcMinMax();
	        image.notifyImageDisplayListeners(null, true);
	        
		} catch (FileNotFoundException e) {
			System.err.println("ERROR: Can't find file "
					+ fileName);
		} catch (IOException e) {

		}
	}
	
}