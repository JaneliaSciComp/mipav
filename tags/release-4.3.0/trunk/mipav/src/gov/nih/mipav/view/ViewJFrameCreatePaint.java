package gov.nih.mipav.view;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import java.awt.*;
import java.awt.event.*;

import java.io.File;
import java.util.*;

import javax.swing.*;


/**
 * Frame that allows the creation of custom paint brushes.  Uses an array of JToggleButtons to create a grid, where the black (on) buttons
 * represent pixels that will be "on" in the Paint Brush Bitset.  Images are saved as .PNG files (tiny, generally less than 1kb) in the
 * user_home/mipav/brushes directory (on a per-user basis).  the built in brushes are stored in the view/icons directory.
 * Saved brushes can be reloaded-overwritten within this editor as well
 * @author linkb
 *
 */
public class ViewJFrameCreatePaint extends JFrame implements ActionListener, MouseListener {

	/** Array of toggle buttons used to create the grid*/
	private JToggleButton [][] buttonGrid;
	
	/** Make sure the button size is always this dimension */
	private final Dimension buttonSize = new Dimension(18,18);
	
	/** grid's height*/
	private int gridHeight = 6;
	
	/** grid's width*/
	private int gridWidth = 6;
	
	/** Panel that holds the grid (stored as private variable so as to remove/rebuild the grid as needed*/
	private JPanel gridPanel = null;
	
	/** The grid resize dialog*/
	private JDialogGridSize gridDialog = null;
	
	/** Menu item that holds the list of paintbrushes to open (for editing)*/
	private JMenu openItem = null;
	
	/** used to create the "selected" state of the buttons*/
	private ImageIcon blackImage = null;
	
	/** used to create the "unselected" state of the buttons*/
	private ImageIcon whiteImage = null;
	
	/** switch that tells the save-dialog that this brush was opened from disk, so brush will be re-written on save*/
	private boolean wasLoaded = false;
	
	/** the name of the loaded brush (used with the above wasLoaded parameter) */
	private String loadName = null;
	
	/**
	 * Main constructor.  Prompts user for grid size and shows the grid
	 *
	 */
	public ViewJFrameCreatePaint() {
		setTitle("Create paint brush");
		try {
		setIconImage(MipavUtil.getIconImage("paint_brush_editor.gif"));
		} catch (Exception e) { }
		gridDialog = new JDialogGridSize(this);
	
		if (gridDialog.wasCancelled()) {
			this.dispose();
			return;
		}
		this.gridHeight = gridDialog.getGridHeight();
		this.gridWidth = gridDialog.getGridWidth();
		
		try {
			blackImage = MipavUtil.getIcon("black.gif");
			whiteImage = MipavUtil.getIcon("white.gif");
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		init();
	}

	/**
	 * Handles action events
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("clearAll")) {
			setAllSelected(false);
		} else if (e.getActionCommand().equals("fillAll")) {
			setAllSelected(true);
		} else if (e.getActionCommand().equals("resizeGrid")) {
			this.setVisible(false);
			gridDialog.setVisible(true);
			
			if (gridDialog.wasCancelled()) {
				this.dispose();
				return;
			}
			
			this.gridHeight = gridDialog.getGridHeight();
			this.gridWidth = gridDialog.getGridWidth();
			buildGrid(null);
			pack();
			setVisible(true);
		} else if (e.getActionCommand().equals("saveBrush")) {
			saveBrush();
		} else if (e.getActionCommand().endsWith(".png")) {
			openBrush(e.getActionCommand());
		}
	}
	
	/** Opens a brush stored on disk (.png)*/
	private void openBrush(String path) {
		FileIO fileIO = new FileIO();
		
		ModelImage brushImage = fileIO.readImage(path);
		
		int [] extents = brushImage.getExtents();
		
		gridWidth = extents[0];
		gridHeight = extents[1];
				
		int [] buffer = new int[gridWidth * gridHeight * 4];
		try {
			brushImage.exportData(0, buffer.length, buffer);
		} catch (Exception e) {
			MipavUtil.displayError("Open brush failed.");
			brushImage.disposeLocal();
			return;
		}
		
		int counter = 0;
		BitSet bitset = new BitSet(gridWidth * gridHeight);
		
		int length = buffer.length;
		
		for ( int i = 0; i < length; i += 4, counter++) {
			if (buffer[i + 1] == 0) {
				bitset.set(counter);
			} else {
				bitset.clear(counter);
			}
			
		}
		
		setVisible(false);
		buildGrid(bitset);
		setVisible(true);
		
		brushImage.disposeLocal();
		wasLoaded = true;
		loadName = new File(path).getName();
	}
	
	/** populates the menu item showing available brushes to edit*/
	private void populateBrushList() {
		openItem.removeAll();
		boolean hasBrush = false;
		JMenuItem brushItem = null;
		
		String userBrushes = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "brushes" + File.separator;
		
		File brushesDir = new File(userBrushes);
		if (brushesDir.isDirectory()) {
			File [] brushes = brushesDir.listFiles();
			
			for (int i = 0; i < brushes.length; i++) {
				
				if (brushes[i].getName().endsWith(".png")) {
					hasBrush = true;
					brushItem = new JMenuItem(brushes[i].getName());
					brushItem.addActionListener(this);
					brushItem.setActionCommand(brushes[i].getPath());
					
					try {
					brushItem.setIcon(new ImageIcon(brushes[i].toURI().toURL()));
					} catch (Exception e){ }
					openItem.add(brushItem);
				}
			}
			
		}
		
		openItem.setEnabled(hasBrush);
	}
	
	/** saves the drawn paintbrush to a .png */
	private void saveBrush() {
		int [] extents = new int[2];
		
		int xStart = -1;
		int yStart = -1;
		int xEnd = 0;
		int yEnd = 0;
		
		
		//find the bounds on the grid image so that we can save a trimmed version to .png
		for (int i = 0; i < gridHeight; i++) {
			for (int j = 0; j < gridWidth; j++) {
				if( buttonGrid[i][j].isSelected()) {
					if (xStart == -1) {
						xStart = j;
					}else {
						if(j < xStart) {
							xStart = j;
						}
					}
					if (yStart == -1) {
						yStart = i;
					}
					if (j > xEnd) {
						xEnd = j;
					}
					
					yEnd = i;
				}
			}
		}
		int width = xEnd - xStart + 1;
		int height = yEnd - yStart + 1;
		
		extents[0] = width;
		extents[1] = height;
		
		ModelImage brushImage = new ModelImage(ModelStorageBase.ARGB, extents, "brush");	
		ViewJFrameImage iFrame = new ViewJFrameImage(brushImage);
		
		BitSet bitset = new BitSet(width * height);
		
		int counter = 0;
		
		
		for (int i = yStart; i <= yEnd; i++) {
			for (int j = xStart; j <= xEnd; j++, counter++) {
				if( buttonGrid[i][j].isSelected()) {
					bitset.set(counter);
				} else {
					bitset.clear(counter);
				}
			}
		}
		
		int [] buffer = new int[4 * width * height];
     

        int i = 0;
        counter = 0;
        Color background = getContentPane().getBackground();
        
        	
        for (int y = 0; y < height; y++) {

            for (int x = 0; x < width; x++, counter++) {
            	
            	if (bitset.get(counter)) {
            		buffer[i] = 0;
            		buffer[i + 1] = 0;
                    buffer[i + 2] = 0;
                    buffer[i + 3] = 0;
            	} else {
            		buffer[i] = 255;
            		buffer[i + 1] = background.getRed();
                    buffer[i + 2] = background.getGreen();
                    buffer[i + 3] = background.getBlue();
            	}
                i += 4;
            }
        }
		
		try {
		brushImage.importData(0, buffer, true);
		} catch (Exception e) {
			e.printStackTrace();
		}
				
		FileIO fileIO = new FileIO();
		
		FileWriteOptions options = new FileWriteOptions(true);
		options.doPutInQuicklist(false);
		options.setFileType(FileUtility.PNG);
		
		String userBrushes = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "brushes" + File.separator;
		
		File brushesDir = new File(userBrushes);
		brushesDir.mkdirs();
		
		options.setFileDirectory(userBrushes);
		if (wasLoaded) {
			options.setFileName(loadName);
			wasLoaded = false;
		} else {
			JFileChooser chooser = new JFileChooser();
	        ViewImageFileFilter filter = new ViewImageFileFilter(new String[] {".png"});

	        chooser.setFileFilter(filter);

	        // if (userInterface.getDefaultDirectory()!=null)
	        chooser.setCurrentDirectory(new File(userBrushes));

	        // else
	        // chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
	        int returnVal = chooser.showSaveDialog(this);

	        if (returnVal == JFileChooser.APPROVE_OPTION) {
	        	loadName = chooser.getSelectedFile().getName();

	        } else {
	            return;
	        }
			
	        if (!loadName.endsWith(".png")) {
	        	loadName = loadName + ".png";
	        }
	        
			options.setFileName(loadName);
		}
		fileIO.writeImage(brushImage, options);
		
		iFrame.close();
		
	}
	
	private void init() {
		JMenuBar menuBar = new JMenuBar();
		JMenu gridMenu = new JMenu("Grid options");
		
		JMenuItem clearAllItem = new JMenuItem("Clear all");
		clearAllItem.setActionCommand("clearAll");
		clearAllItem.addActionListener(this);
		
		JMenuItem fillAllItem = new JMenuItem("Fill all");
		fillAllItem.setActionCommand("fillAll");
		fillAllItem.addActionListener(this);
		
		JMenuItem resizeGridItem = new JMenuItem("Resize grid");
		resizeGridItem.setActionCommand("resizeGrid");
		resizeGridItem.addActionListener(this);
		
		JMenuItem saveItem = new JMenuItem("Save paint brush");
		saveItem.setActionCommand("saveBrush");
		saveItem.addActionListener(this);
		
		openItem = new JMenu("Load paint brush");
		populateBrushList();
		
		
		gridMenu.add(fillAllItem);
		gridMenu.add(clearAllItem);
		gridMenu.add(resizeGridItem);
		gridMenu.add(saveItem);
		gridMenu.add(openItem);
		
		menuBar.add(gridMenu);
		
		setJMenuBar(menuBar);
		
		addMouseListener(this);
		
		buildGrid(null);
		
		JPanel directionPanel = new JPanel();
		directionPanel.add(WidgetFactory.buildLabel("Left-mouse to draw, right-mouse to erase"));
		
		getContentPane().add(directionPanel, BorderLayout.SOUTH);
		pack();
		MipavUtil.centerOnScreen(this);
		setVisible(true);
	}
	
	private void setAllSelected(boolean selected) {
		for (int i = 0; i < gridHeight; i++) {
			for (int j = 0; j < gridWidth; j++) {
				buttonGrid[i][j].setSelected(selected);
			}
		}
	}
	

	public void mouseEntered(MouseEvent mouseEvent) {
		processMouse(mouseEvent);
	}
	
	public void mouseExited(MouseEvent mouseEvent) {
		
	}
	
	public void mouseClicked(MouseEvent e){
		
	}
	public void mousePressed(MouseEvent mouseEvent) {
		processMouse(mouseEvent);
	}
	public void mouseReleased(MouseEvent mouseEvent) {
	}
	
	/** handles the toggling function of the grid (on/off)*/
	private void processMouse(MouseEvent mouseEvent) {
		int mouseMods = mouseEvent.getModifiers();
		
		if (mouseMods == MouseEvent.BUTTON1_MASK) {
			if (mouseEvent.getSource() instanceof JToggleButton) {
				((JToggleButton)mouseEvent.getSource()).setSelected(true);
			}
			
		} else if (mouseMods == MouseEvent.BUTTON3_MASK) {
			if (mouseEvent.getSource() instanceof JToggleButton) {
				((JToggleButton)mouseEvent.getSource()).setSelected(false);
			}
		}
	}
	
	/**
	 * Builds the grid, with or without a pre-loaded BitSet (from an on-disk paintbrush)
	 * @param preLoad BitSet that can be null, if passed in then the grid will be built based on the on-disk .png file
	 */
	private void buildGrid(BitSet preLoad) {
		
		if (gridPanel != null) {
			gridPanel.removeAll();
		} else {
			gridPanel = new JPanel(new GridBagLayout());
		}
		
		buttonGrid = null;
		
		buttonGrid = new JToggleButton[gridHeight][gridWidth];
		
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.anchor = GridBagConstraints.CENTER;
		gbc.gridx = 0;
		gbc.gridy = 0;
		
		int counter = 0;
		for (int i = 0; i < gridHeight; i++, gbc.gridy++) {
			
			for (int j = 0; j < gridWidth; j++, counter++, gbc.gridx++) {
				buttonGrid[i][j] = new JToggleButton();
				
				buttonGrid[i][j].setSize(buttonSize);
				buttonGrid[i][j].setPreferredSize(buttonSize);
				
				buttonGrid[i][j].setIcon(whiteImage);
				buttonGrid[i][j].setSelectedIcon(blackImage);
				buttonGrid[i][j].setDisabledIcon(whiteImage);
				buttonGrid[i][j].setDisabledSelectedIcon(blackImage);
				buttonGrid[i][j].addMouseListener(this);
				buttonGrid[i][j].setEnabled(false);
				
				if (preLoad != null && preLoad.get(counter)) {
					buttonGrid[i][j].setSelected(true);
				}
			
				
				gridPanel.add(buttonGrid[i][j], gbc);
			}
			gbc.gridx = 0;
		}
		
		getContentPane().add(gridPanel, BorderLayout.CENTER);
		pack();
	}

	
	/**
	 * Simple dialog to prompt user for grid size
	 * @author linkb
	 *
	 */
	private class JDialogGridSize extends JDialogBase {
				
		private JTextField widthField;
		private JTextField heightField;
		
		private int height = 6;
		private int width = 6;
		
		private boolean wasCancelled = false;
		
		public JDialogGridSize(JFrame frame) {
			super(frame, true);
			try {
				setIconImage(MipavUtil.getIconImage("paint_brush_editor.gif"));
				} catch (Exception e) { }
			init();
			wasLoaded = false;
			
		}
		
					
		public void actionPerformed(ActionEvent e) {
			if (e.getActionCommand().equalsIgnoreCase("OK")) {
				if (setVariables()) {
					setVisible(false);
				}
			} else if (e.getActionCommand().equalsIgnoreCase("Cancel")) {
				setVisible(false);
				wasCancelled = true;
			}
			
		}
		
		public boolean wasCancelled() {
			return wasCancelled;
		}
		
		public int getGridHeight() {
			return height;
		}
		public int getGridWidth() {
			return width;
		}
		
		private boolean setVariables() {
			
			try {
				height = Integer.parseInt(heightField.getText());
				width = Integer.parseInt(widthField.getText());
				
				if (height < 1 || width < 1) {
					MipavUtil.displayError("Width and height must be greater than 1");
					return false;
				}
			} catch (Exception e ) {
				return false;
			}
			
			return true;
		}
		
		private void init() {
			setTitle("Choose grid size");
			JPanel mainPanel = new JPanel();
			PanelManager pm = new PanelManager(mainPanel);
			
			widthField = WidgetFactory.buildTextField("12");
			MipavUtil.makeNumericsOnly(widthField, false);
			
			heightField = WidgetFactory.buildTextField("12");
			MipavUtil.makeNumericsOnly(heightField, false);
			
			JLabel heightLabel = WidgetFactory.buildLabel("Grid height: ");
			JLabel widthLabel = WidgetFactory.buildLabel("Grid width: ");
			
			pm.add(widthLabel);
			pm.add(widthField);
			
			pm.addOnNextLine(heightLabel);
			pm.add(heightField);
			
			mainPanel.setBorder(buildTitledBorder(""));
			
			getContentPane().add(mainPanel, BorderLayout.CENTER);
			
			getContentPane().add(buildButtons(), BorderLayout.SOUTH);
			pack();
			
			MipavUtil.centerOnScreen(this);
			
			setVisible(true);
		}
		
	}
}