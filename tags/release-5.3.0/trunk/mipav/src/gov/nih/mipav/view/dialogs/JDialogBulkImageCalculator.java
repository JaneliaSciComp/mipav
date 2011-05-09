package gov.nih.mipav.view.dialogs;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewTableModel;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * 
 * @author pandyan
 *
 */
public class JDialogBulkImageCalculator extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** source image. **/
    private ModelImage imageA;
    
    /** result image **/
    private ModelImage resultImage;
    
    /** is color boolean **/
    private boolean isColor = false;
    
    /** operator combo box **/
    private JComboBox comboBoxOperator;
    
    /** table model for the srcimages. **/
    private ViewTableModel srcTableModel;
    
    /** table to display the src image names. **/
    private JTable srcImagesTable;
    
    /** list of images to send to algorithm **/
    private ArrayList<ModelImage> srcImagesList;
    
    /** list of any additional images browsed to  **/
    private ArrayList<ModelImage> additionalImagesList = new ArrayList<ModelImage>();
    
    /** clip radio button **/
    //private JRadioButton radioClip;

    /** promote radio button **/
    //private JRadioButton radioPromote;
    
    /** clip mode **/
    private int clipMode = AlgorithmImageMath.PROMOTE;
    
    /** handle to algorithm **/
    private AlgorithmImageCalculator alg;
    
    /** operation type....for now set to ADD **/
    private int opType = AlgorithmImageCalculator.ADD;
    
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
    
    
	
	
	/**
	 * empty constructor..needed for scripting
	 *
	 */
	public JDialogBulkImageCalculator() {
		
	}
	
	/**
	 * 
	 * @param theParentFrame
	 * @param im
	 */
	public JDialogBulkImageCalculator(Frame theParentFrame, ModelImage im) {
		 super(theParentFrame, false);
	        imageA = im;
	        isColor = im.isColorImage();
	        init();
	        
            
	}
	
	/**
	 *  action performed
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		 if (command.equals("OK")) {
			 callAlgorithm();

	     } else if (command.equals("Cancel")) {
	    	 if(additionalImagesList.size() > 0) {
	         	for(int i=0;i<additionalImagesList.size();i++) {
	     			ModelImage addImage = ((ModelImage)additionalImagesList.get(i));
	     			addImage.disposeLocal();
	     			addImage = null;
	     		}
	         }
	         dispose();
	     } else if (command.equals("Help")) {
	            MipavUtil.showHelp("U4070");
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
	         	Vector<String> rowData = new Vector<String>();
                rowData.add(addImage.getImageName());
                srcTableModel.addRow(rowData);
                additionalImagesList.add(addImage);
	         	srcImagesList.add(addImage);
	         } 
	     }else if(command.equals("removeSelected")) {
	    	 // cannot remove first entry..since this is imageA
	    	 if(srcImagesTable.getSelectedRow() == 0) {
	    		 MipavUtil.displayError("Can not remove the first entry since this is the image in which the dialog was opened on");
	    		 return;
	    	 }
	    	 selectedRows = srcImagesTable.getSelectedRows();
	    	 //need at least 2 images for algorithm to work
	    	 if((srcImagesTable.getRowCount() - selectedRows.length) < 2) {
	    		 MipavUtil.displayError("At least 2 images need to be present for algorithm to operate");
	    		 return; 
	    	 }
	    	 for(int i=(selectedRows.length-1);i>=0;i--) {
	    		 //need to remove from the table
	             srcTableModel.removeRow(selectedRows[i]);
	             // need to remove that entry from the List
	             srcImagesList.remove(selectedRows[i]); 
	    	 }
	    	 
	    	
	     }

	}
	
	/**
	 *  call algorithm
	 */
	protected void callAlgorithm() {

		resultImage = new ModelImage(imageA.getType(), imageA.getExtents(), makeImageName(imageA.getImageName(), "_calc"));
		if (isColor && opType == AlgorithmImageCalculator.AVGERAGE_WITH_STDEV){
		    MipavUtil.displayError("Cannot do St Dev with Color Images!");
		    return;
		}
		Object[] objs = srcImagesList.toArray();
		ModelImage[] srcImages = new ModelImage[objs.length];
		for (int i = 0; i < objs.length; i++) {
			srcImages[i] = (ModelImage)objs[i];
		}
		
		 try {
		
			 alg = new AlgorithmImageCalculator(resultImage,srcImages,opType,clipMode);
			 
			 
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
             MipavUtil.displayError("Dialog Bulk Image Calculator: unable to allocate enough memory");

             return;
         }
		
		
		
	}
	
	
	/**
	 *  algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if (algorithm instanceof AlgorithmImageCalculator) {

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
                if(additionalImagesList.size() > 0) {
                	for(int i=0;i<additionalImagesList.size();i++) {
            			ModelImage addImage = ((ModelImage)additionalImagesList.get(i));
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
        setTitle("Image Calculator - Bulk Images");
        
        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);

        inputPanel.setBorder(buildTitledBorder("<operation>  on  Images"));
        
        JLabel labelOperator = new JLabel("Operation:");
        labelOperator.setForeground(Color.black);
        labelOperator.setFont(serif12);
        
        comboBoxOperator = new JComboBox();
        comboBoxOperator.addItemListener(this);
        comboBoxOperator.setFont(serif12);
        comboBoxOperator.setBackground(Color.white);
        comboBoxOperator.addItem("Add");
        comboBoxOperator.addItem("Average");
        comboBoxOperator.addItem("Average w/ Std Dev");
        comboBoxOperator.addItem("Minimum");
        comboBoxOperator.addItem("Maximum");
        
        JPanel srcPanel = new JPanel();
        srcTableModel = new ViewTableModel();
        srcTableModel.addColumn("Images");
        srcImagesTable = new JTable(srcTableModel);
        srcImagesTable.setPreferredScrollableViewportSize(new Dimension(250, 100));
        JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);
        srcPanel.add(srcImagesScrollPane);
        
        //populate with images of like dimensionality that are open in MIPAV
        Vector<String> rowData = new Vector<String>();
        rowData.add(imageA.getImageName());
        srcTableModel.addRow(rowData);
        srcImagesList = new ArrayList<ModelImage>();
        srcImagesList.add(imageA);
        populateImagesTable();
        
        additionalImagesLabel = new JLabel(" Add Additional Images: ");
        additionalImagesLabel.setForeground(Color.black);
        additionalImagesLabel.setFont(serif12);
        addImageBrowseButton = new JButton("Browse");
        addImageBrowseButton.addActionListener(this);
        addImageBrowseButton.setActionCommand("addImageBrowse");
        
        
        removeSelectedImagesLabel = new JLabel(" Remove Selected Images: ");
        removeSelectedImagesLabel.setForeground(Color.black);
        removeSelectedImagesLabel.setFont(serif12);
        removeSelectedButton = new JButton("Remove");
        removeSelectedButton.addActionListener(this);
        removeSelectedButton.setActionCommand("removeSelected");
        
        /*ButtonGroup group = new ButtonGroup();
        radioClip = new JRadioButton("Clip", true);
        radioClip.setFont(serif12);
        group.add(radioClip);
        radioPromote = new JRadioButton("Promote destination image type", false);
        radioPromote.setFont(serif12);
        group.add(radioPromote);
        */
        
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
        inputPanel.add(labelOperator, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(comboBoxOperator, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        inputPanel.add(srcPanel, gbc);
        gbc.insets = new Insets(5, 5, 15, 5);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        inputPanel.add(additionalImagesLabel, gbc);
        gbc.gridx = 1;
        inputPanel.add(addImageBrowseButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        inputPanel.add(removeSelectedImagesLabel, gbc);
        gbc.gridx = 1;
        inputPanel.add(removeSelectedButton, gbc);
        /*gbc.gridx = 0;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.NONE;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridy = 4;
        inputPanel.add(radioClip, gbc);
        gbc.gridy = 5;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        inputPanel.add(radioPromote, gbc);
        */
        
        getContentPane().add(inputPanel);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        //setResizable(false);
        pack();
        setVisible(true);
        
    }
    
    
    
    /**
     * populate images table
     * Builds a list of images to operate on from the template image.
     */
    private void populateImagesTable() {
        int j;
        ViewUserInterface UI;
        boolean sameDims = true;


        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        // Guaranteed to have at least one unique potential image B, because it's
        // tested for in ViewJFrameImage before this dialog is created.
        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();
            sameDims = true;

            if (!imageA.getImageName().equals(name)) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(img) != null) {

                    if (imageA.getNDims() == img.getNDims()) {

                        for (j = 0; j < imageA.getNDims(); j++) {

                            if (imageA.getExtents()[j] != img.getExtents()[j]) {
                                sameDims = false;
                            }
                        }

                        if ((sameDims == true) && (isColor == img.isColorImage())) {
                            Vector<String> rowData = new Vector<String>();
                            rowData.add(img.getImageName());
                            srcTableModel.addRow(rowData);
                            srcImagesList.add(img);
                        }
                    }
                }
            }
        }
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
		for (int i=1;i<=size;i++) {
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

		setOperator(scriptParameters.getParams().getInt("operator_type"));
        setClipMode(scriptParameters.getParams().getInt("data_type_clip_mode"));

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

        scriptParameters.getParams().put(ParameterFactory.newParameter("operator_type", opType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("data_type_clip_mode", clipMode));

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
     * set operator.
     *
     * @param  n  operator type
     */
    public void setOperator(int n) {
        opType = n;
    }
    
    
    /**
     * set clip mode.
     *
     * @param  n  the clip mode to be used when performing the math algorithm
     */
    public void setClipMode(int n) {
        clipMode = n;
    }
	
	/**
     * item staate changed
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == comboBoxOperator) {

            if (comboBoxOperator.getSelectedIndex() == 0) {
                opType = AlgorithmImageCalculator.ADD;
            }else if (comboBoxOperator.getSelectedIndex() == 1) {
                opType = AlgorithmImageCalculator.AVERAGE;
            }else if (comboBoxOperator.getSelectedIndex() == 2) {
                opType = AlgorithmImageCalculator.AVGERAGE_WITH_STDEV;
            }else if (comboBoxOperator.getSelectedIndex() == 3) {
                opType = AlgorithmImageCalculator.MINIMUM;
            }else if (comboBoxOperator.getSelectedIndex() == 4) {
                opType = AlgorithmImageCalculator.MAXIMUM;
            }
            
            
            
        }
    }



    
    /**
     *  windoe closing
     */
    public void windowClosing(WindowEvent event) {
        if(additionalImagesList.size() > 0) {
        	for(int i=0;i<additionalImagesList.size();i++) {
    			ModelImage addImage = ((ModelImage)additionalImagesList.get(i));
    			addImage.disposeLocal();
    			addImage = null;
    		}
        }
        dispose();
    }

	
	
	


}
