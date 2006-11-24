import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

/**
 * @author pandyan
 * 
 * 
 * This class is the main dialog for this plugin that
 * registers multiple source images to a target image
 * and then averages them to get a final registered image
 * 
 *
 */
public class PlugInDialogImageAverageRegistration extends JDialogBase implements AlgorithmInterface{
	
	/**  table to display the src image names*/
	private JTable srcImagesTable;
	/**  table to store the target image name*/
	private JTable targetImageTable;
	/**   table model for the srcimages*/
	private DefaultTableModel srcTableModel;
	/**   table model for the targetImage*/
	private DefaultTableModel targetTableModel;
	/**   array to store the srcImage files*/
	private File[] srcFiles;
	/**   array to store the target image file*/
	private File targetFile;
	/**   arraylist to store the src images*/
	private ArrayList srcImages = new ArrayList();
	/**   model image for target and result images*/
	private ModelImage targetImage, resultImage;
    /**   handle to the registration options dialog*/
    private PlugInImageAverageRegistration_RegOptionsDialog regOptions;
    /**   handle to the algorithm*/
    private PlugInAlgorithmImageAverageRegistration alg;
    /**   boolean to determine if target image will be used in final average calculation*/
    private boolean includeTargetImageinCalc;
    /**   boolean to determine if the result image should be saved*/
    private boolean saveResultImage;
    /**   result image name*/
    private String resultImageName = "";
    /**   boolean that determines if image is colr or not*/
    private boolean doColor;
    /**   checkbox for including target image in final average calc*/
    private JCheckBox includeTargetCheckBox;
    /**   checkbox for saving result image*/
    private JCheckBox saveAsCheckBox;
    /**   button that brings up registration options dialog*/
    private JButton registrationOptionsButton;
    /**   input box for populating resultImageName*/
    private JTextField saveAsTextField;
    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    /** DOCUMENT ME! */
    private String directory;
    
	
	/**
	 * constructor
	 * @param modal
	 */
	public PlugInDialogImageAverageRegistration(boolean modal) {
		super(modal);
		userInterface = ViewUserInterface.getReference();
		init();
	}
	
	/**
	 * init method
	 *
	 */
	public void init() {
		setForeground(Color.black);
        setTitle("Image Average Registration");
		
		GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);
        
        
		
		JPanel srcPanel = new JPanel();
		srcTableModel = new DefaultTableModel();
		srcTableModel.addColumn("Source Images");
		srcImagesTable = new JTable(srcTableModel);
		srcImagesTable.setPreferredScrollableViewportSize(new Dimension(300, 200));
		JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);
		srcImagesTable.addMouseListener(new MouseHandler());
		srcPanel.add(srcImagesScrollPane);
		JButton srcBrowseButton = new JButton("Browse");
		srcBrowseButton.addActionListener(this);
		srcBrowseButton.setActionCommand("srcBrowse");
		srcPanel.add(srcBrowseButton);
		
        JPanel targetPanel = new JPanel();
		targetTableModel = new DefaultTableModel();
		targetTableModel.addColumn("Target Image");
		targetImageTable = new JTable(targetTableModel);
		targetImageTable.addMouseListener(new MouseHandler());
		targetImageTable.setPreferredScrollableViewportSize(new Dimension(300, 30));
		JScrollPane targetImageScrollPane = new JScrollPane(targetImageTable);
		targetPanel.add(targetImageScrollPane);
		JButton targetBrowseButton = new JButton("Browse");
		targetBrowseButton.addActionListener(this);
		targetBrowseButton.setActionCommand("targetBrowse");
		targetPanel.add(targetBrowseButton);
		
		GridBagLayout prefsPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints prefsPanelConstraints = new GridBagConstraints();
		JPanel prefsPanel = new JPanel(prefsPanelGridBagLayout);
		includeTargetCheckBox = new JCheckBox("Include target image in averaging calculation");
		registrationOptionsButton = new JButton("Registration Options");
		registrationOptionsButton.addActionListener(this);
		registrationOptionsButton.setActionCommand("options");
		registrationOptionsButton.setEnabled(false);
		prefsPanelConstraints.gridx = 0;
		prefsPanelConstraints.gridy = 0;
		prefsPanelConstraints.gridwidth = 2;
		prefsPanelConstraints.anchor = GridBagConstraints.CENTER;
		prefsPanelConstraints.insets = new Insets(15,0,15,0);
		prefsPanelGridBagLayout.setConstraints(registrationOptionsButton, prefsPanelConstraints);
		prefsPanelConstraints.gridx = 0;
		prefsPanelConstraints.gridy = 1;
		prefsPanelConstraints.gridwidth = 2;
		prefsPanelConstraints.anchor = GridBagConstraints.WEST;
		prefsPanelConstraints.insets = new Insets(0,0,0,0);
		prefsPanelGridBagLayout.setConstraints(includeTargetCheckBox, prefsPanelConstraints);
		saveAsCheckBox = new JCheckBox("Save result image as  ");
		saveAsCheckBox.addActionListener(this);
		saveAsCheckBox.setActionCommand("saveAs");
		prefsPanelConstraints.gridx = 0;
		prefsPanelConstraints.gridy = 2;
		prefsPanelConstraints.gridwidth = 1;
		prefsPanelConstraints.anchor = GridBagConstraints.WEST;
		prefsPanelConstraints.insets = new Insets(0,0,10,0);
		prefsPanelGridBagLayout.setConstraints(saveAsCheckBox, prefsPanelConstraints);
		saveAsTextField = new JTextField(15);
		saveAsTextField.setEnabled(false);
		prefsPanelConstraints.gridx = 1;
		prefsPanelConstraints.gridy = 2;
		prefsPanelConstraints.gridwidth = 1;
		prefsPanelConstraints.insets = new Insets(0,0,10,0);
		prefsPanelGridBagLayout.setConstraints(saveAsTextField, prefsPanelConstraints);
		prefsPanel.add(registrationOptionsButton);
		prefsPanel.add(includeTargetCheckBox);
		prefsPanel.add(saveAsCheckBox);
		prefsPanel.add(saveAsTextField);
		
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 0;
		mainPanelGridBagLayout.setConstraints(srcPanel, mainPanelConstraints);
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 1;
		mainPanelGridBagLayout.setConstraints(targetPanel, mainPanelConstraints);
		mainPanelConstraints.gridx = 0;
		mainPanelConstraints.gridy = 2;
		mainPanelGridBagLayout.setConstraints(prefsPanel, mainPanelConstraints);
		mainPanel.add(srcPanel);
		mainPanel.add(targetPanel);
		mainPanel.add(prefsPanel);
		JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setResizable(false);
        setVisible(true);
	}
	
	/**
	 * action performed method
	 * @param e
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		String filename = "";
		
		FileIO fileIO = null;
		String userDir = "";
		if(command.equalsIgnoreCase("srcBrowse")) {
			JFileChooser srcFileChooser = new JFileChooser();
			if (userInterface.getDefaultDirectory() != null) {
				srcFileChooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
            } else {
            	srcFileChooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
			srcFileChooser.setMultiSelectionEnabled(true);
			int srcReturnVal = srcFileChooser.showOpenDialog(this);
			if (srcReturnVal == JFileChooser.APPROVE_OPTION) {
				
				srcFiles = srcFileChooser.getSelectedFiles();
				directory = String.valueOf(srcFileChooser.getCurrentDirectory()) + File.separatorChar;
				userInterface.setDefaultDirectory(directory);
				for (int k = 0; k < srcFiles.length; k++) {
					try {
						fileIO = new FileIO();
						filename = srcFiles[k].getName();
						ModelImage srcImage = fileIO.readImage(filename, directory, false, null); 
						if (srcImage == null) {
		                    System.err.println("Error loading file");

		                    return;
		                }
						Vector rowData = new Vector();
						rowData.add(filename);
						srcTableModel.addRow(rowData);
						srcImages.add(srcImage);
						srcImage = null;
					}
					catch(OutOfMemoryError err) {
		                MipavUtil.displayError("Out of memory!");
		                return;
					}
					
					
				}
			}
		}
		else if(command.equalsIgnoreCase("targetBrowse")) {
			JFileChooser targetFileChooser = new JFileChooser();
			if (userInterface.getDefaultDirectory() != null) {
				targetFileChooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
            } else {
            	targetFileChooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
			targetFileChooser.setMultiSelectionEnabled(false);
			int targetReturnVal = targetFileChooser.showOpenDialog(this);
			if(targetReturnVal == JFileChooser.APPROVE_OPTION) {
				try {
					targetFile = targetFileChooser.getSelectedFile();
					fileIO = new FileIO();
					filename = targetFile.getName();
					directory = String.valueOf(targetFileChooser.getCurrentDirectory()) + File.separatorChar;
					userInterface.setDefaultDirectory(directory);
					targetImage = fileIO.readImage(filename, directory, false, null); 
					if (targetImage == null) {
	                    System.err.println("Error loading file");

	                    return;
	                }
					Vector rowData = new Vector();
					rowData.add(filename);
					if(targetTableModel.getRowCount() == 0) {
						targetTableModel.addRow(rowData);
					}
					else {
						targetTableModel.setValueAt(filename, 0, 0);
					}
					
					//need to test if target image is color or not
					if (targetImage.isColorImage()) {
			            doColor = true;
			        } else {
			            doColor = false;
			        }
					
					//instantiate the dialog...but it is not visible yet....in case user just wants defaults
					regOptions = new PlugInImageAverageRegistration_RegOptionsDialog(doColor);
					
					//enable the Registration Options Button
					registrationOptionsButton.setEnabled(true);
					
					
					
				}
				catch(OutOfMemoryError err) {
					MipavUtil.displayError("Out of memory!");
		            return;
				}
			}
		}
		else if(command.equalsIgnoreCase("cancel")) {
			if(regOptions != null) {
				regOptions.dispose();
			}
			finalize();
			dispose();
		}
		else if(command.equalsIgnoreCase("ok")) {
			if(srcImages.size()==0 || targetImage==null) {
				MipavUtil.displayError("Both Source Image(s) and Target Image are required");
				return;
			}
			//loop through src images and make sure they are same as target image in terms of color
			for(int i=0;i<srcImages.size();i++) {
				if(((ModelImage)srcImages.get(i)).isColorImage() != doColor) {
					MipavUtil.displayError("Source Image(s) and Target Image must either both be color images or grey scale images");
					return;
				}
			}
			int targetNumDims = targetImage.getNDims();
			for(int i=0;i<srcImages.size();i++) {
				int srcNumDims = ((ModelImage)srcImages.get(i)).getNDims();
				if(srcNumDims != targetNumDims) {
					MipavUtil.displayError("Source Image(s) and Target Image must have the same dimensions");
					return;
				}	
			}
			
			if(regOptions.setVariables()) {
				includeTargetImageinCalc = includeTargetCheckBox.isSelected();
				saveResultImage = saveAsCheckBox.isSelected();
				if(saveResultImage) {
					resultImageName = saveAsTextField.getText();
					if(resultImageName.trim().equals("")) {
						MipavUtil.displayError("Result Image Name is required");
						return;
					}
				}

				callAlgorithm();
				if(regOptions != null) {
					regOptions.dispose();
				}

			}
			
		}
		else if(command.equalsIgnoreCase("options")) {
			regOptions.setVisible(true);
		}
		else if(command.equalsIgnoreCase("saveAs")) {
			if(saveAsCheckBox.isSelected()) {
				saveAsTextField.setEnabled(true);
			}
			else {
				saveAsTextField.setText("");
				saveAsTextField.setEnabled(false);
			}
		}
		

	}
	
	/**
	 * callAlgorithm method
	 *
	 */
	protected void callAlgorithm() {
		
		//call the algorithm
		alg = new PlugInAlgorithmImageAverageRegistration(srcImages,targetImage,regOptions.getCost(),regOptions.getDOF(),regOptions.getInterp(),regOptions.getInterp2(),
																								  regOptions.getRotateBeginX(),regOptions.getRotateEndX(),regOptions.getCoarseRateX(),
																								  regOptions.getFineRateX(),regOptions.getRotateBeginY(),regOptions.getRotateEndY(),
																								  regOptions.getCoarseRateY(),regOptions.getFineRateY(),regOptions.getRotateBeginZ(),
																								  regOptions.getRotateEndZ(),regOptions.getCoarseRateZ(),regOptions.getFineRateZ(),
																								  regOptions.isMaxOfMinResol(),includeTargetImageinCalc);
		
		alg.addListener(this);
		
		
        setVisible(false);
        
        if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
		
		
		
	}
	
	/**
	 * algorithmPerformed Method
	 * This is called after the algorithm is finished
	 * 
	 * @param algorithm
	 *
	 * 
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		//grab the result image
		resultImage = alg.getResultImage();
		
		if(resultImage == null) {
			MipavUtil.displayError("Result Image is null");
			finalize();
			dispose();
			return;
		}
		
		//display result image if they did not select save
	
		if(!saveAsCheckBox.isSelected()) {
			try {
				new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
	        } 
			catch (OutOfMemoryError error) {
	        	MipavUtil.displayError("Out of memory: unable to open new frame");
	        }
		}
        
		
		//need to save image here if they selected the checkbox
		if(saveAsCheckBox.isSelected()) {
			if(!resultImageName.equals("")) {
				FileWriteOptions options = new FileWriteOptions(resultImageName,directory,true);
				//resultImage.getParentFrame().save(options, -1);
				FileIO io = new FileIO();
				io.writeImage(resultImage, options);
				resultImage.disposeLocal();
				resultImage = null;
			}
				
		}
		
		finalize();
		dispose();
		

		
	}


	public void windowClosing(WindowEvent event) {
		super.windowClosing(event);
		if(regOptions != null) {
			regOptions.dispose();
		}
		finalize();
		dispose();
	}
	
	
	
	public void finalize() {
		for(int i=0;i<srcImages.size();i++) {
			if ((ModelImage)srcImages.get(i) != null) {
				((ModelImage)srcImages.get(i)).disposeLocal();
				srcImages.set(i,null);
			}
		}
		if(targetImage != null) {
			targetImage.disposeLocal();
			targetImage = null;
		}
		System.gc();
    }
	
	
	/**
     * This is an inner class that is used by the tables within the dialogs
     * to delete src and target images from the tables
     */
    private class MouseHandler extends MouseAdapter implements ActionListener {
    	
    	/**   point representing where the mouse event took place */
    	Point p;
    	/**   popup menu for removing files */
    	JPopupMenu popupMenu;
    	/**    rowIndex*/
    	int rowIndex;
    	/**    DefaultTableModel for the corresponding table*/
    	DefaultTableModel model;
    	/**    name of the image file*/
    	String imageName;
    	/**    menu item in the popup menu*/
    	JMenuItem menuItem;
    	/**    column name of the corresponding table*/
    	String columnName;
    	
    	/**
    	 * mouseClicked 
    	 * @param e
    	 */
        public void mouseClicked(MouseEvent e) {
        	if (e.getButton() == MouseEvent.BUTTON3) {
        		p = new Point(e.getX(),e.getY());
        		popupMenu = new JPopupMenu();
        		rowIndex = ((JTable)e.getSource()).rowAtPoint(p);
        		model = (DefaultTableModel)((JTable)e.getSource()).getModel();
        		imageName = (String)model.getValueAt(rowIndex, 0);
        		menuItem = new JMenuItem("Remove " + imageName);
        		menuItem.addActionListener(this);
                menuItem.setActionCommand("remove");
                popupMenu.add(menuItem);
                popupMenu.show((JTable)e.getSource(), e.getX(), e.getY());
        		columnName = ((JTable)e.getSource()).getColumnName(0);
        	}
        }
        
        /**
         * actionPerformed
         * @param event
         */
        public void actionPerformed(ActionEvent event) {
        	String command = event.getActionCommand();
        	if(command.equals("remove")) {
        		if(columnName.equals("Source Images")) {
        			//need to remove from the table and the ArrayList
        			PlugInDialogImageAverageRegistration.this.srcTableModel.removeRow(rowIndex);
        			((ModelImage)PlugInDialogImageAverageRegistration.this.srcImages.get(rowIndex)).disposeLocal();
        			PlugInDialogImageAverageRegistration.this.srcImages.set(rowIndex, null);
        			PlugInDialogImageAverageRegistration.this.srcImages.remove(rowIndex);
        		}
        		else if(columnName.equals("Target Image")) {
        			//need to remove from table, make targetImage to null, set the regOptions instance to null, and disable regOptions button
        			PlugInDialogImageAverageRegistration.this.targetTableModel.removeRow(rowIndex);
        			PlugInDialogImageAverageRegistration.this.targetImage.disposeLocal();
        			PlugInDialogImageAverageRegistration.this.targetImage = null;
        			PlugInDialogImageAverageRegistration.this.registrationOptionsButton.setEnabled(false);
        			PlugInDialogImageAverageRegistration.this.regOptions = null;
        		}
        		
        	}
        		
        }

    	
    }
	
	
	

}
