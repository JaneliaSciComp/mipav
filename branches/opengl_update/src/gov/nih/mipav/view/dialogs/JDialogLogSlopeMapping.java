package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmLogSlopeMapping;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewTableModel;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;

public class JDialogLogSlopeMapping extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** source image. **/
    private ModelImage imageA;
    
    /** result image **/
    private ModelImage resultImage;
    
    /** is color boolean **/
    private boolean isColor = false;
    
    /** table model for the srcimages. **/
    private ViewTableModel srcTableModel;
    
    /** table to display the src image names. **/
    private JTable srcImagesTable;
    
    /** list of images to send to algorithm **/
    private ArrayList<ModelImage> srcImagesList;
    
    /** list of x values to send to algorithm **/
    private ArrayList<Double> xValuesList;
    
    /** handle to algorithm **/
    private AlgorithmLogSlopeMapping alg;
    
    /** result image in new frame **/
    private int displayLoc = NEW;
    
    /** label for browsing for additional images **/
    private JLabel additionalImagesLabel;
    
    /** browse button **/
    private JButton addImageBrowseButton;
    
    /** label for removing selected images **/
    private JLabel removeSelectedImagesLabel;
    
    /** remove button **/
    private JButton removeSelectedButton;
    
    /** boolean isMultifile **/
    private boolean isMultifile;
    
    /** indices of selcted rows to remove **/
    private int[] selectedRows;
    
    private int imagesAdded = 0;
    
    private int xValuesAdded = 0;
    
    private JButton xValueButton;
    
    private JTextField xValueText;
    
    private double xValue;
	
	
	public JDialogLogSlopeMapping() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}
	
	
	
	/**
	 *  action performed
	 */
	public void actionPerformed(ActionEvent event) {
		String tmpStr;
		String command = event.getActionCommand();
		 if (command.equals("OK")) {
			 if (imagesAdded < 2) {
				 MipavUtil.displayError("Must have at least 2 images");
				 return;
			 }
			 if (xValuesAdded < imagesAdded) {
				 MipavUtil.displayError("Must have an x value for the last image");
				 return;
			 }
			 callAlgorithm();

	     } else if (command.equals("Cancel")) {
	    	 if(srcImagesList.size() > 0) {
	         	for(int i=0;i<srcImagesList.size();i++) {
	     			ModelImage addImage = ((ModelImage)srcImagesList.get(i));
	     			addImage.disposeLocal();
	     			addImage = null;
	     		}
	         }
	         dispose();
	     } else if (command.equals("Help")) {
	            MipavUtil.showHelp("");
	     } else if (command.equals("addImageBrowse")) {
	    	 ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
	         JFileChooser chooser = fileChooser.getFileChooser();
	         if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                 chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
             } else {
                 chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
             }
	         chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
	         chooser.setDialogTitle("Choose image");
	         int returnValue = chooser.showOpenDialog(this);
	         if (returnValue == JFileChooser.APPROVE_OPTION) { 	
	         	FileIO fileIO = new FileIO();
	         	isMultifile = fileChooser.isMulti();
	         	ModelImage addImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator, isMultifile, null);
	         	if (imagesAdded == 0) {
	         		if (addImage.isColorImage()) {
	         		    MipavUtil.displayError("Image cannot be a color image");
	         		    addImage.disposeLocal();
	         		    addImage = null;
	         		    return;
	         		}
	         		else if (addImage.isComplexImage()) {
	         			MipavUtil.displayError("Image cannot be a complex image");
	         		    addImage.disposeLocal();
	         		    addImage = null;
	         		    return;	
	         		}
	         		else if (addImage.getType() == ModelStorageBase.BOOLEAN) {
	         			MipavUtil.displayError("Image cannot be a BOOLEAN image");
	         		    addImage.disposeLocal();
	         		    addImage = null;
	         		    return;		
	         		}
	         		else {
	         	        imageA = addImage;
	         		}
	         	} // if (imagesAdded == 0)
	         	else {
	         	if(addImage.getNDims() != imageA.getNDims()) {
	         		MipavUtil.displayError("Image does not have proper dimensions");
	         		addImage.disposeLocal();
	         		addImage = null;
	 				return;
	         	}
	         	if(imageA.getNDims() == 2) {
	         		if(!(imageA.getExtents()[0] == addImage.getExtents()[0] && imageA.getExtents()[1] == addImage.getExtents()[1])) {
	         			MipavUtil.displayError("Image does not have proper extents");
	         			addImage.disposeLocal();
		         		addImage = null;
		         		return;
	         		}
	         	}
	         	if(imageA.getNDims() == 3) {
	         		if(!(imageA.getExtents()[0] == addImage.getExtents()[0] && imageA.getExtents()[1] == addImage.getExtents()[1] && imageA.getExtents()[2] == addImage.getExtents()[2])) {
	         			MipavUtil.displayError("Image does not have proper extents");
	         			addImage.disposeLocal();
		         		addImage = null;
		         		return;
	         		}
	         	}
	         	if(imageA.getNDims() == 4) {
	         		if(!(imageA.getExtents()[0] == addImage.getExtents()[0] && imageA.getExtents()[1] == addImage.getExtents()[1] && imageA.getExtents()[2] == addImage.getExtents()[2] && imageA.getExtents()[3] == addImage.getExtents()[3])) {
	         			MipavUtil.displayError("Image does not have proper extents");
	         			addImage.disposeLocal();
		         		addImage = null;
		         		return;
	         		}
	         	}
	         	if(isColor != addImage.isColorImage()) {
	         		MipavUtil.displayError("Image color properties are not correct");
	         		if(addImage != null) {
	         			addImage.disposeLocal();
	         		}
	         		addImage = null;
	 				return;
	         	}
	         	} // else
	         	Vector<String> rowData = new Vector<String>();
                rowData.add(addImage.getImageName());
                srcTableModel.addRow(rowData);
	         	srcImagesList.add(addImage);
	         	additionalImagesLabel.setEnabled(false);
	         	addImageBrowseButton.setEnabled(false);
	         	xValueButton.setEnabled(true);
	         	xValueText.setEnabled(true);
	         	imagesAdded++;
	         } 
	     } else if (command.equals("addXValue")) {
	    	 tmpStr = xValueText.getText();
	    	 try {
	             xValue = Double.parseDouble(tmpStr);
	    	 }
	    	 catch (NumberFormatException e) {
	    		 MipavUtil.displayError("Number format exception on attempt to parse x value");
	    		 return;
	    	 }
	         Vector<String> rowData = new Vector<String>();
	         rowData.add(tmpStr);
	         srcTableModel.addRow(rowData);
	         xValuesList.add(Double.valueOf(xValue));
	    	 xValueButton.setEnabled(false);
	    	 xValueText.setEnabled(false);
	    	 additionalImagesLabel.setEnabled(true);
	    	 addImageBrowseButton.setEnabled(true);
	    	 xValuesAdded++;
	     }else if(command.equals("removeSelected")) {
	    	 selectedRows = srcImagesTable.getSelectedRows();
	    	 for(int i=(selectedRows.length-1);i>=0;i--) {
	    		 //need to remove from the table
	    		 if ((selectedRows[i] % 2) == 0) {
	            	 // Row represents an image
	    		     // Remove x value if it exists
	    		     if (xValuesAdded == imagesAdded) {
	            	     srcTableModel.removeRow(selectedRows[i]+1);
	            	     xValuesList.remove(selectedRows[i]/2);
	            	     xValuesAdded--;
	    		     }
	            	 // Remove image
	            	 srcTableModel.removeRow(selectedRows[i]);
	            	 srcImagesList.remove(selectedRows[i]/2); 
	            	 imagesAdded--;
	             } // if ((selectedRows[i] % 2) == 0)
	    		 else {
	    			 // Row represents an x value
	    			 srcTableModel.removeRow(selectedRows[i]);
            	     xValuesList.remove(selectedRows[i]/2);
            	     xValuesAdded--;
            	     srcTableModel.removeRow(selectedRows[i]-1);
	            	 srcImagesList.remove(selectedRows[i]/2); 
	            	 imagesAdded--;
	            	 if (((i - 1) >= 0) && (selectedRows[i] == ((selectedRows[i-1]) + 1))) {
	            		 i--;
	            	 }
	    		 } // else
	             // need to remove that entry from the List
	    	 }
	    	 
	    	
	     }

	}
	
	/**
	 *  call algorithm
	 */
	protected void callAlgorithm() {

		resultImage = new ModelImage(ModelStorageBase.DOUBLE, imageA.getExtents(), makeImageName(imageA.getImageName(), "_logSlope"));
		Object[] objs = srcImagesList.toArray();
		ModelImage[] srcImages = new ModelImage[objs.length];
		for (int i = 0; i < objs.length; i++) {
			srcImages[i] = (ModelImage)objs[i];
		}
		Object[] objs2 = xValuesList.toArray();
		double[] xValueArray = new double[objs2.length];
		for (int i = 0; i < objs2.length; i++) {
			xValueArray[i] = ((Double)objs2[i]).doubleValue();
		}
		
		 try {
		
			 alg = new AlgorithmLogSlopeMapping(resultImage,srcImages,xValueArray);
			 
			 
			 //This is very important. Adding this object as a listener allows the algorithm to
             // notify this object when it has completed of failed. See algorithm performed event.
             // This is made possible by implementing AlgorithmedPerformed interface
             alg.addListener(this);

             createProgressBar(imageA.getImageName(), alg);

             // Hide dialog
             setVisible(false);

             if (isRunInSeparateThread()) {

                 // Start the thread as a low priority because we wish to still have user interface work fast.
                 if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                     MipavUtil.displayError("A thread is already running on this object");
                 }
             } else {
                 alg.run();
             }
         } catch (OutOfMemoryError x) {

             if (resultImage != null) {
                 resultImage.disposeLocal(); // Clean up memory of result image
                 resultImage = null;
             }

             System.gc();
             MipavUtil.displayError("Dialog Log Slope Mapping: unable to allocate enough memory");

             return;
         }
		
		
		
	}
	
	
	/**
	 *  algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if (algorithm instanceof AlgorithmLogSlopeMapping) {

            if ((alg.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(imageA, resultImage);

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
                
                //dispose of any additionally opened images
                if(srcImagesList.size() > 0) {
                	for(int i=0;i<srcImagesList.size();i++) {
            			ModelImage addImage = ((ModelImage)srcImagesList.get(i));
            			addImage.disposeLocal();
            			addImage = null;
            		}
                }
               
            }
		}
		
		if (algorithm.isCompleted()) {
            insertScriptLine();
        }
		alg.finalize();
        alg = null;
		dispose();
	}
	
	
	
	/**
	 * init
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	setForeground(Color.black);
        setTitle("Log Slope Mapping");
        
        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);

        inputPanel.setBorder(buildTitledBorder("Adding Images and X Values"));
        
        
        
        JPanel srcPanel = new JPanel();
        srcTableModel = new ViewTableModel();
        srcTableModel.addColumn("Image - X Value pairs");
        srcImagesTable = new JTable(srcTableModel);
        srcImagesTable.setPreferredScrollableViewportSize(new Dimension(250, 100));
        JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);
        srcPanel.add(srcImagesScrollPane);
        
        srcImagesList = new ArrayList<ModelImage>();
        xValuesList = new ArrayList<Double>();
        
        additionalImagesLabel = new JLabel(" Add Images: ");
        additionalImagesLabel.setForeground(Color.black);
        additionalImagesLabel.setFont(serif12);
        addImageBrowseButton = new JButton("Browse");
        addImageBrowseButton.addActionListener(this);
        addImageBrowseButton.setActionCommand("addImageBrowse");
        
        xValueButton = new JButton("Add X Value:");
        xValueButton.addActionListener(this);
        xValueButton.setActionCommand("addXValue");
        xValueButton.setEnabled(false);
        
        xValueText = new JTextField(10);
        xValueText.setFont(serif12);
        xValueText.setForeground(Color.black);
        xValueText.addFocusListener(this);
        xValueText.setEnabled(false);
        
        removeSelectedImagesLabel = new JLabel(" Remove Image X Value pairs: ");
        removeSelectedImagesLabel.setForeground(Color.black);
        removeSelectedImagesLabel.setFont(serif12);
        removeSelectedButton = new JButton("Remove");
        removeSelectedButton.addActionListener(this);
        removeSelectedButton.setActionCommand("removeSelected");
        
        JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        OKCancelPanel.add(cancelButton);
        buildHelpButton();
        OKCancelPanel.add(helpButton);
        
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        inputPanel.add(srcPanel, gbc);
        gbc.insets = new Insets(5, 5, 15, 5);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        inputPanel.add(additionalImagesLabel, gbc);
        gbc.gridx = 1;
        inputPanel.add(addImageBrowseButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        inputPanel.add(xValueButton, gbc);
        gbc.gridx = 1;
        inputPanel.add(xValueText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        inputPanel.add(removeSelectedImagesLabel, gbc);
        gbc.gridx = 1;
        inputPanel.add(removeSelectedButton, gbc);
        
        getContentPane().add(inputPanel);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        //setResizable(false);
        pack();
        setVisible(true);
        
    }
    
    
    
    
	

	

	/**
	 * set GUI from params
	 */
	protected void setGUIFromParams() {
		imageA = scriptParameters.retrieveInputImage(1);
		srcImagesList = new ArrayList<ModelImage>();
		srcImagesList.add(imageA);
		isColor = imageA.isColorImage();
		int size = scriptParameters.getParams().getInt("size");
		for (int i=2;i<=size;i++) {
			if(scriptParameters.retrieveInputImage(i)!= null) {
				ModelImage addImage = scriptParameters.retrieveInputImage(i);
				
				
				if(addImage.getNDims() != imageA.getNDims()) {
	         		MipavUtil.displayError("all images do not have same dimensions");
	         		throw new IllegalArgumentException();
	         	}
	         	if(imageA.getNDims() == 2) {
	         		if(!(imageA.getExtents()[0] == addImage.getExtents()[0] && imageA.getExtents()[1] == addImage.getExtents()[1])) {
	         			MipavUtil.displayError("all images do not have same extents");
		         		throw new IllegalArgumentException();
	         		}
	         	}
	         	if(imageA.getNDims() == 3) {
	         		if(!(imageA.getExtents()[0] == addImage.getExtents()[0] && imageA.getExtents()[1] == addImage.getExtents()[1] && imageA.getExtents()[2] == addImage.getExtents()[2])) {
	         			MipavUtil.displayError("all images do not have same extents");
		         		throw new IllegalArgumentException();
	         		}
	         	}
	         	if(imageA.getNDims() == 4) {
	         		if(!(imageA.getExtents()[0] == addImage.getExtents()[0] && imageA.getExtents()[1] == addImage.getExtents()[1] && imageA.getExtents()[2] == addImage.getExtents()[2] && imageA.getExtents()[3] == addImage.getExtents()[3])) {
	         			MipavUtil.displayError("all images do not have same extents");
		         		throw new IllegalArgumentException();
	         		}
	         	}
	         	if(isColor != addImage.isColorImage()) {
	         		MipavUtil.displayError("all images do not have same color info");
	         		throw new IllegalArgumentException();
	         	}
	         	srcImagesList.add(addImage);	
			}
		}
	
		xValuesList = new ArrayList<Double>();
		for (int i = 1; i <= size; i++) {
			xValuesList.add(Double.valueOf(scriptParameters.getParams().getDouble("x_value" + i)));
		}


	}

	/**
	 * store params from gui
	 */
	protected void storeParamsFromGUI() throws ParserException {
		for(int i=0;i<srcImagesList.size();i++) {
			scriptParameters.storeInputImage((ModelImage)srcImagesList.get(i));
		}
		scriptParameters.getParams().put(ParameterFactory.newParameter("size", srcImagesList.size()));
		scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
		for (int i = 0; i < xValuesList.size(); i++) {
			scriptParameters.getParams().put(ParameterFactory.newParameter("x_value" + i,((Double)xValuesList.get(i)).doubleValue()));
		}

	}
	
	 /**
     * get result image
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
    
    
	
	/**
     * item staate changed
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        
    }



    
    /**
     *  windoe closing
     */
    public void windowClosing(WindowEvent event) {
        if(srcImagesList.size() > 0) {
        	for(int i=0;i<srcImagesList.size();i++) {
    			ModelImage addImage = ((ModelImage)srcImagesList.get(i));
    			addImage.disposeLocal();
    			addImage = null;
    		}
        }
        dispose();
    }

	


	
}
