package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConcatMult3Dto4D;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewTableModel;
import gov.nih.mipav.view.ViewUserInterface;







/**
 * @author pandyan
 * This utilty concats multiple 3D images of same type to a 4D image
 *
 */
public class JDialogConcatMult3Dto4D extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** images to concat...the ones that are open in mipav **/
	private ArrayList<ModelImage> imagesToConcat;
	
	/** table model **/
	private ViewTableModel srcTableModel;
	
	/** table **/
	private JTable srcImagesTable;
	
	/** additional images to concat...the ones you add later on**/
	private ArrayList<ModelImage> additionalImagesToConcat;
	
	/** indices of selcted rows to remove **/
    private int selectedRow;
	
    /** algorithm **/
    private AlgorithmConcatMult3Dto4D alg;
    
    /** destination image **/
    private ModelImage destImage;
	
	
	/**
	 * empty constructor..needed for scripting
	 *
	 */
	public JDialogConcatMult3Dto4D() {
		
	}
	
	/**
	 * 
	 * @param theParentFrame
	 * @param im
	 */
	public JDialogConcatMult3Dto4D(Frame theParentFrame, ArrayList<ModelImage> imagesToConcat) {
		 super(theParentFrame, false);
		 this.imagesToConcat = imagesToConcat;
		 additionalImagesToConcat = new ArrayList<ModelImage>();
	     init();
	        
            
	}
	
	
	
	
	/**
	 * init
	 */
	private void init() {
		setForeground(Color.black);
        setTitle("Concat Multiple 3D Images to 4D");
        
        JPanel mainPanel = new JPanel(new GridBagLayout());

        srcTableModel = new ViewTableModel();
        srcTableModel.addColumn("Images");
        srcImagesTable = new JTable(srcTableModel);
        srcImagesTable.setPreferredScrollableViewportSize(new Dimension(300, 400));
        srcImagesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);

		for(int i=0;i<imagesToConcat.size();i++) {
			
			Vector<String> rowData = new Vector<String>();
            rowData.add(imagesToConcat.get(i).getImageName());
            srcTableModel.addRow(rowData); 
            
		}
		srcImagesTable.setRowSelectionInterval(0, 0);

		JButton moveUpButton = new JButton("Move Up");
		moveUpButton.addActionListener(this);
		moveUpButton.setActionCommand("moveUp");
		
		JButton moveDownButton = new JButton("Move Down");
		moveDownButton.addActionListener(this);
		moveDownButton.setActionCommand("moveDown");
		
		JPanel movePanel = new JPanel(new GridBagLayout());
		GridBagConstraints gbc2 = new GridBagConstraints();
		
		gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridheight = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.anchor = GridBagConstraints.CENTER;
        gbc2.insets = new Insets(5, 5, 5, 5);
		movePanel.add(moveUpButton, gbc2);
		
		gbc2.gridx = 1;
        gbc2.gridy = 0;
        gbc2.gridheight = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.anchor = GridBagConstraints.CENTER;
        gbc2.insets = new Insets(5, 5, 5, 5);
        movePanel.add(moveDownButton, gbc2);
		
		JLabel additionalImagesLabel = new JLabel(" Add Additional Images: ");
        additionalImagesLabel.setForeground(Color.black);
        additionalImagesLabel.setFont(serif12);
        JButton addImageBrowseButton = new JButton("Browse");
        addImageBrowseButton.addActionListener(this);
        addImageBrowseButton.setActionCommand("addImageBrowse");

        JLabel removeSelectedImagesLabel = new JLabel(" Remove Selected Images: ");
        removeSelectedImagesLabel.setForeground(Color.black);
        removeSelectedImagesLabel.setFont(serif12);
        JButton removeSelectedButton = new JButton("Remove");
        removeSelectedButton.addActionListener(this);
        removeSelectedButton.setActionCommand("removeSelected");

		JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        OKCancelPanel.add(cancelButton);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridwidth = 2;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(srcImagesScrollPane, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(movePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(additionalImagesLabel, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(addImageBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(removeSelectedImagesLabel, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(removeSelectedButton, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        pack();
		setMinimumSize(this.getSize());
		setVisible(true);
	}
	
	
	
	
	
	

	/**
	 * calls algorithm
	 */
	protected void callAlgorithm() {
		
		ModelImage[] images = new ModelImage[srcTableModel.getRowCount()];
		
		for(int i=0;i<srcTableModel.getRowCount();i++) {
			
			
			String imageName = (String)srcTableModel.getValueAt(i, 0);
			boolean found = false;
			for(int k=0;k<imagesToConcat.size();k++) {
				if(imageName.equals(imagesToConcat.get(k).getImageName())) {
					found = true;
					images[i] = imagesToConcat.get(k);
					break;
				}
			}
			if(!found) {
				for(int k=0;k<additionalImagesToConcat.size();k++) {
					if(imageName.equals(additionalImagesToConcat.get(k).getImageName())) {
						found = true;
						images[i] = additionalImagesToConcat.get(k);
						break;
					}
				}
			}

		}

		 int[] destExtents = new int[4];
         destExtents[0] = images[0].getExtents()[0];
         destExtents[1] = images[0].getExtents()[1];
         destExtents[2] = images[0].getExtents()[2];
         destExtents[3] = images.length;
		
		destImage = new ModelImage(images[0].getType(), destExtents, makeImageName(images[0].getImageName(), "_concat"));
		
		alg = new AlgorithmConcatMult3Dto4D(images, destImage);
		
		
		 alg.addListener(this);

         createProgressBar("", alg);
         
         if (isRunInSeparateThread()) {

             // Start the thread as a low priority because we wish to still have user interface work fast.
             if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                 MipavUtil.displayError("A thread is already running on this object");
             }

         } else {

             alg.run();

         }
		

	}

	/**
	 * set gui
	 */
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	/**
	 * store params
	 */
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof AlgorithmConcatMult3Dto4D) {
			if (alg.isCompleted() == true) {
				new ViewJFrameImage(destImage);
				cleanup();
				dispose();
			}
		}

	}
	
	/**
	 * cleanup
	 */
	private void cleanup() {
		for(int i=0;i<additionalImagesToConcat.size();i++) {
			ModelImage img = additionalImagesToConcat.get(i);
			img.disposeLocal();
			img = null;
		}
	}

	/**
	 * action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if (command.equals("addImageBrowse")) {
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

	         FileIO fileIO = new FileIO();
	         boolean isMultifile = fileChooser.isMulti();
	         ModelImage img = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator, isMultifile, null);
	         	
	         ModelImage activeImage = imagesToConcat.get(0);
	         	
	         if (activeImage.getNDims() == img.getNDims() && activeImage.getExtents()[0] == img.getExtents()[0] &&
          			 activeImage.getExtents()[1] == img.getExtents()[1] &&
          			 activeImage.getExtents()[2] == img.getExtents()[2] && 
          			 img.getDataType() == activeImage.getDataType()) {

	        	     additionalImagesToConcat.add(img);
          			 Vector<String> rowData = new Vector<String>();
                     rowData.add(img.getImageName());
                     srcTableModel.addRow(rowData); 

             }else {
            	 MipavUtil.displayError("Image is not of same dimensions, extents or data type");
            	 if(img != null) {
            		 img.disposeLocal();
            		 img = null;
            	 }
             }
		}else if(command.equals("removeSelected")) {

	    	 selectedRow = srcImagesTable.getSelectedRow();
	    	 //need at least 2 images for algorithm to work
	    	 if((srcImagesTable.getRowCount() - 1) < 2) {
	    		 MipavUtil.displayError("At least 2 images need to be present for algorithm to operate");
	    		 return; 
	    	 }
	             // need to remove that entry from the List
	    		 boolean found = false;
	             for(int k=0;k<additionalImagesToConcat.size();k++) {
	            	 String name = additionalImagesToConcat.get(k).getImageName();
	            	 
	            	 if(name.equals(srcTableModel.getValueAt(selectedRow, 0))) {
	            		 ModelImage removeImage = additionalImagesToConcat.remove(k);
	            		 removeImage.disposeLocal();
	            		 removeImage = null;
	            		 found = true;
	            		 break;
	            	 }
	            	 
	             }
	             if(!found) {
	            	 for(int k=0;k<imagesToConcat.size();k++) {
		            	 String name = imagesToConcat.get(k).getImageName();
		            	 
		            	 if(name.equals(srcTableModel.getValueAt(selectedRow, 0))) {
		            		 imagesToConcat.remove(k);
		            		 found = true;
		            		 break;
		            	 }
		            	 
		             }
	             }
	             srcTableModel.removeRow(selectedRow);
	             
	             int newCount = srcImagesTable.getRowCount();
	             if(selectedRow == newCount) {
	            	 int lastIndex = srcImagesTable.getRowCount() - 1;
	            	 srcImagesTable.setRowSelectionInterval(lastIndex, lastIndex);
	             }else {
	            	 srcImagesTable.setRowSelectionInterval(selectedRow, selectedRow);
	             }
	    	 
	    	
	     }else if(command.equals("moveDown")) {
	    	 selectedRow = srcImagesTable.getSelectedRow();
	    	 if(selectedRow != srcImagesTable.getRowCount()-1) {
	    		 srcTableModel.moveRow(selectedRow, selectedRow, selectedRow + 1);
	    		 srcImagesTable.setRowSelectionInterval(selectedRow + 1, selectedRow + 1);
	    	 }
	    	 
	    	 
	     }else if(command.equals("moveUp")) {
	    	 selectedRow = srcImagesTable.getSelectedRow();
	    	 selectedRow = srcImagesTable.getSelectedRow();
	    	 if(selectedRow != 0) {
	    		 srcTableModel.moveRow(selectedRow, selectedRow, selectedRow - 1);
	    		 srcImagesTable.setRowSelectionInterval(selectedRow - 1, selectedRow - 1);
	    	 }
	     }else if(command.equals("OK")) {
	    	 if((srcImagesTable.getRowCount()) < 2) {
	    		 MipavUtil.displayError("At least 2 images need to be present for algorithm to operate");
	    		 return; 
	    	 }
	    	 callAlgorithm();
	     }

	}
	
	
	
	

}
