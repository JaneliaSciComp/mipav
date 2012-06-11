package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConcatMult3Dto3D;
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


public class JDialogConcatMult3Dto3D extends JDialogScriptableBase implements
		AlgorithmInterface {

	
	/** available images to concat...the ones that are open in mipav **/
	private ArrayList<ModelImage> availableImagesToConcat;
	
	/** images to concat **/
	private ArrayList<ModelImage> imagesToConcat;
	
	/** table model **/
	private ViewTableModel srcTableModel, destTableModel;
	
	/** table **/
	private JTable srcImagesTable, destImagesTable;
	
	/** additional images to concat...the ones you add later on**/
	private ArrayList<ModelImage> additionalImagesToConcat;
	
	/** indices of selcted rows to remove **/
    private int selectedRow;
	
    /** algorithm **/
    private AlgorithmConcatMult3Dto3D alg;
    
    /** destination image **/
    private ModelImage destImage;


	/**
	 * empty constructor..needed for scripting
	 *
	 */
	public JDialogConcatMult3Dto3D() {
		
	}
	
	/**
	 * 
	 * @param theParentFrame
	 * @param im
	 */
	public JDialogConcatMult3Dto3D(Frame theParentFrame, ArrayList<ModelImage> imagesToConcat) {
		 super(theParentFrame, false);
		 this.availableImagesToConcat = imagesToConcat;
		 additionalImagesToConcat = new ArrayList<ModelImage>();
		 imagesToConcat = new ArrayList<ModelImage>();
	     init();
	        
            
	}
	
	
	
	
	/**
	 * init
	 */
	private void init() {
		setForeground(Color.black);

		setTitle("Concat Multiple 3D Images to One 3D");

        
        JPanel mainPanel = new JPanel(new GridBagLayout());

        srcTableModel = new ViewTableModel();
        srcTableModel.addColumn("Available Images");
        srcTableModel.addColumn("");
        srcImagesTable = new JTable(srcTableModel);
        srcImagesTable.setPreferredScrollableViewportSize(new Dimension(400, 500));
        srcImagesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        
        JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);
        
        destTableModel = new ViewTableModel();
        destTableModel.addColumn("Images to concatenate");
        destImagesTable = new JTable(destTableModel);
        destImagesTable.setPreferredScrollableViewportSize(new Dimension(400, 500));
        destImagesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        
        JScrollPane destImagesScrollPane = new JScrollPane(destImagesTable);
        

		for(int i=0;i<availableImagesToConcat.size();i++) {
			
			Vector<Object> rowData = new Vector<Object>();
            rowData.add(availableImagesToConcat.get(i).getImageName());

            srcTableModel.addRow(rowData); 
            
		}
		srcImagesTable.setRowSelectionInterval(0, 0);
		
		srcImagesTable.getColumn("").setCellRenderer(new JButtonCellRenderer());
		srcImagesTable.getColumn("").setMaxWidth(60);
		//srcImagesTable.getColumn("").setCellEditor(new JButtonCellEditor());
		srcImagesTable.addMouseListener( new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if(e.getClickCount() == 1) {
					JTable source = (JTable)e.getSource();
					int row = source.rowAtPoint(e.getPoint());
					int column = source.columnAtPoint(e.getPoint());
					if(column == 1) {

						String imgName = (String)(srcImagesTable.getValueAt(row, 0));
						String rightImgName = "";
			            int rightTableRowCount = destImagesTable.getRowCount();
			            boolean found = false;
			            for(int i=0;i<rightTableRowCount;i++) {
			            	rightImgName = (String)(destImagesTable.getValueAt(i, 0));
			            	if(imgName.equals(rightImgName)) {
			            		found = true;
			            		destTableModel.removeRow(i);
			            		
			            		int selectedRow = destImagesTable.getSelectedRow();
			            		if(destImagesTable.getRowCount() > 0) {
			            			if(selectedRow == -1) {
			            				if(destImagesTable.getRowCount() == 1) {
			            					destImagesTable.setRowSelectionInterval(0, 0);
			            				}else {
					            			if(i == 0) {
					            				destImagesTable.setRowSelectionInterval(0, 0);
					            			}else {
					            				destImagesTable.setRowSelectionInterval(i-1, i-1);
					            			}
					            		}	
				            		}
			            		}
			            		
			            		break;
			            	}
			            }
						if(!found) {
							//put it on the list on the right
							Vector<Object> rowData = new Vector<Object>();
				            rowData.add(imgName);
				            destTableModel.addRow(rowData); 
				            int destRowCount = destImagesTable.getRowCount();
				            destImagesTable.setRowSelectionInterval(destRowCount-1, destRowCount-1);
						}

						srcImagesTable.repaint();
						destImagesTable.repaint();

					}
					
				}
				
				
			}
		
		
		});
		
		

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
        
        
        JPanel additionalImagesPanel = new JPanel(new GridBagLayout());
		GridBagConstraints gbc3 = new GridBagConstraints();
		gbc3.gridx = 0;
		gbc3.gridy = 0;
		gbc3.gridheight = 1;
		gbc3.fill = GridBagConstraints.BOTH;
		gbc3.anchor = GridBagConstraints.CENTER;
		gbc3.insets = new Insets(5, 5, 5, 5);
		additionalImagesPanel.add(additionalImagesLabel, gbc3);
		
		gbc3.gridx = 1;
		gbc3.gridy = 0;
		gbc3.gridheight = 1;
		gbc3.fill = GridBagConstraints.BOTH;
		gbc3.anchor = GridBagConstraints.CENTER;
		gbc3.insets = new Insets(5, 5, 5, 5);
		additionalImagesPanel.add(addImageBrowseButton, gbc3);
		

        /*JLabel removeSelectedImagesLabel = new JLabel(" Remove Selected Images: ");
        removeSelectedImagesLabel.setForeground(Color.black);
        removeSelectedImagesLabel.setFont(serif12);
        JButton removeSelectedButton = new JButton("Remove");
        removeSelectedButton.addActionListener(this);
        removeSelectedButton.setActionCommand("removeSelected");*/

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
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(srcImagesScrollPane, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(destImagesScrollPane, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        //gbc.gridwidth = 2;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 15, 5);
        mainPanel.add(additionalImagesPanel, gbc);
        
        

        gbc.gridx = 1;
        gbc.gridy = 1;
        //gbc.gridwidth = 2;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 15, 5);
        mainPanel.add(movePanel, gbc);
        
        
        

        /*gbc.gridx = 0;
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
        mainPanel.add(addImageBrowseButton, gbc);*/

       /* gbc.gridx = 0;
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
        mainPanel.add(removeSelectedButton, gbc);*/

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
		
		ModelImage[] images = new ModelImage[destTableModel.getRowCount()];
		
		for(int i=0;i<destTableModel.getRowCount();i++) {
			
			
			String imageName = (String)destTableModel.getValueAt(i, 0);
			boolean found = false;
			for(int k=0;k<availableImagesToConcat.size();k++) {
				if(imageName.equals(availableImagesToConcat.get(k).getImageName())) {
					found = true;
					images[i] = availableImagesToConcat.get(k);
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

		

			int [] destExtents = new int[3];
	         destExtents[0] = images[0].getExtents()[0];
	         destExtents[1] = images[0].getExtents()[1];
	         int zLength = 0;
	         for (int i=0;i<images.length;i++) {
	        	 zLength = zLength + images[i].getExtents()[2];
	         }
	         destExtents[2] = zLength;
	  


		destImage = new ModelImage(images[0].getType(), destExtents, makeImageName(images[0].getImageName(), "_concat"));
		
		alg = new AlgorithmConcatMult3Dto3D(images, destImage);
		
		
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
		if(algorithm instanceof AlgorithmConcatMult3Dto3D) {
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
	         
	         
	         if (returnValue == JFileChooser.APPROVE_OPTION) {

		         FileIO fileIO = new FileIO();
		         boolean isMultifile = fileChooser.isMulti();
		         ModelImage img = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator, isMultifile, null);
		         	
		         ModelImage activeImage = availableImagesToConcat.get(0);
		         	
		         if (activeImage.getNDims() == img.getNDims() && activeImage.getExtents()[0] == img.getExtents()[0] &&
	          			 activeImage.getExtents()[1] == img.getExtents()[1] &&
	          			 activeImage.getExtents()[2] == img.getExtents()[2] && 
	          			 img.getDataType() == activeImage.getDataType()) {
	
		        	     additionalImagesToConcat.add(img);
	          			 Vector<String> rowData = new Vector<String>();
	                     rowData.add(img.getImageName());
	                     srcTableModel.addRow(rowData); 
	                     
	                     int srcRowCount = srcImagesTable.getRowCount();
				         srcImagesTable.setRowSelectionInterval(srcRowCount-1, srcRowCount-1);
	
	             }else {
	            	 MipavUtil.displayError("Image is not of same dimensions, extents or data type");
	            	 if(img != null) {
	            		 img.disposeLocal();
	            		 img = null;
	            	 }
	             }
	         }
		}/*else if(command.equals("removeSelected")) {

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
	            	 for(int k=0;k<availableImagesToConcat.size();k++) {
		            	 String name = availableImagesToConcat.get(k).getImageName();
		            	 
		            	 if(name.equals(srcTableModel.getValueAt(selectedRow, 0))) {
		            		 availableImagesToConcat.remove(k);
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
	    	 
	    	
	     }*/else if(command.equals("moveDown")) {
	    	 if(destImagesTable.getRowCount() >= 2 && destImagesTable.getSelectedRow() != -1) {
		    	 selectedRow = destImagesTable.getSelectedRow();
		    	 if(selectedRow != destImagesTable.getRowCount()-1) {
		    		 destTableModel.moveRow(selectedRow, selectedRow, selectedRow + 1);
		    		 destImagesTable.setRowSelectionInterval(selectedRow + 1, selectedRow + 1);
		    	 }
	    	 }
	    	 
	    	 
	     }else if(command.equals("moveUp")) {
	    	 if(destImagesTable.getRowCount() >= 2 && destImagesTable.getSelectedRow() != -1) {
		    	 selectedRow = destImagesTable.getSelectedRow();
		    	 selectedRow = destImagesTable.getSelectedRow();
		    	 if(selectedRow != 0) {
		    		 destTableModel.moveRow(selectedRow, selectedRow, selectedRow - 1);
		    		 destImagesTable.setRowSelectionInterval(selectedRow - 1, selectedRow - 1);
		    	 }
	    	 }
	     }else if(command.equals("OK")) {
	    	 if((destImagesTable.getRowCount()) < 2) {
	    		 MipavUtil.displayError("At least 2 images need to be present in the \" Images to concatenate \" table for algorithm to operate");
	    		 return; 
	    	 }
	    	 callAlgorithm();
	     }else if(command.equals("Cancel")) {
	    	 cleanup();
	    	 dispose();
	     }

	}
	
	
	
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private class JButtonCellRenderer extends DefaultTableCellRenderer {
		 public Component getTableCellRendererComponent(final JTable table, final Object value,
	                final boolean isSelected, final boolean hasFocus, final int row, final int column) {
			 
			 
	            JButton comp;
	            
	            String imgName = (String)(srcImagesTable.getValueAt(row, 0));
	            
	            int rightTableRowCount = destImagesTable.getRowCount();
	            boolean found = false;
	            for(int i=0;i<rightTableRowCount;i++) {
	            	String rightImgName = (String)(destImagesTable.getValueAt(i, 0));
	            	if(imgName.equals(rightImgName)) {
	            		found = true;
	            		break;
	            	}
	            	
	            }
	            
	            if(found) {
	            	
	            	comp = new JButton("<<<");
	            	
	            	 comp.setForeground(Color.red);
	            	
	            
	            }else {
	            	comp = new JButton(">>>");
	            }
	            setHorizontalAlignment(SwingConstants.CENTER);

	

	            return comp;
	        }
	}
	



}
