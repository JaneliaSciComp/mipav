package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomSQ;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileImageXML;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.JTreeDICOM;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.PreviewImageContainer;
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.ViewFileChooserSubsample;
import gov.nih.mipav.view.ViewJComponentPreviewImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewTableModel;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.IOException;
import java.text.NumberFormat;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.JTableHeader;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;


/**
 * Simple dialog used to show the contents of a selected DICOMDIR file. Allows the user to view
 * previews of the image, related tags, along with opening the image.
 *
 */
public class JDialogDicomDir extends JDialogBase implements  ActionListener,  ItemListener,
TreeSelectionListener, ChangeListener, PreviewImageContainer {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3861014709972568409L;

    /** Column names for data provenance*/
    public static final String [] dpColumnNames = new String[] {"Time","Action","JVM","Mipav","User"};

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Describes the initial size of the textual display area when the dialog is created. The value is given in pixel
     * size rather than the number of characters since the display area has no characters to display.
     */
    protected final Dimension DEFAULT_SIZE = new Dimension(1005, 705);
    /** DOCUMENT ME! */
    private int origBrightness = 0;

    /** DOCUMENT ME! */
    private float origContrast = 1;

    /** DOCUMENT ME! */
    private JPanel buttonPanel;

    /** DOCUMENT ME! */
    private JScrollPane scrollPane; // here so we can set scroll correct

    private boolean ifSuccess = false;
    
    JPanel imagePanel = new JPanel();
    
    ViewJComponentPreviewImage img;
    
    /** The table model */
    private ViewTableModel tagViewer;
    
    private JTreeDICOM tree;
    
    /** DOCUMENT ME! */
    private JPanel brightPanel;
    
    /** DOCUMENT ME! */
    protected Font serif12, serif12B;
    
    /** DOCUMENT ME! */
    private JSlider brightSlider, contSlider;
    
    
    

    /** DOCUMENT ME! */
    private JSplitPane imageSliderPane;
    
    /** DOCUMENT ME! */
    private Dimension defaultImageSize;
    
    /** DOCUMENT ME! */
    private JLabel current, current2;
    
    /** DOCUMENT ME! */
    private JPanel sliderPanel = null;
    
    /** DOCUMENT ME! */
    private NumberFormat nfc;
    
    /** DOCUMENT ME! */
    protected FileImageXML.Thumbnail thumbnail = null;
    
    /** DOCUMENT ME! */
    private float contrast = 1;
    
    /** DOCUMENT ME! */
    private int brightness = 0;
 
    DefaultMutableTreeNode base;
     
    private FileDicomSQ dirInfo;
    
    private FileInfoDicom dicomInfo;
    
    FileDicom opener = null;
    
    private File file;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor for DICOMDIR
     */
    public JDialogDicomDir(Frame parent) {
        super(parent, false);
        // get the selected directory
        JFileChooser chooser ;
        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
        	chooser = new JFileChooser(ViewUserInterface.getReference().getDefaultDirectory());
        } else {
        	chooser = new JFileChooser(System.getProperties().getProperty("user.dir"));
        }
        
        
    	chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        chooser.setDialogTitle("Open the DICOMDIR file.");
        

        
        int returnValue = chooser.showOpenDialog(parent);
	    File file = null;
	    
        if (returnValue == ViewFileChooserSubsample.APPROVE_OPTION)
        	file = chooser.getSelectedFile();

        if (file != null) {
        	if  (file.getName().toLowerCase().startsWith("dicomdir"))
        	{
                this.file = file;
                
        		try {
        			opener = new FileDicom(file.getName(), file.getParent()+File.separatorChar);
        			opener.readHeader(true);
        		} catch (IOException e) {
        			MipavUtil.displayError(e.toString());
        		}
                init("DICOMDIR Browser");
                scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
                ifSuccess = true;
        	}
        	else
        		MipavUtil.displayError("File must be named 'DICOMDIR'");
        }
        

    }
    public JDialogDicomDir(Frame parent, File file, FileDicom caller) {
        super(parent, false);
        if (this.runInSeparateThread==false){
        	
	        
	        this.opener = caller;
	        if (file != null) {
	                this.file = file;
	                init("DWDIR Browser");
	                scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
	                ifSuccess = true;
	        }
        }

    }
        

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the "Close" button is pressed.
     * Opens either a new file or selected images based on button.
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        System.out.println("working8");

        if (source == cancelButton) {
            dispose();
        } else if (event.getActionCommand().equals("Open Images")) {
        	openImage();
        } else if (event.getActionCommand().equals("Open DICOMDIR")) {
        	open();
        } else {
            super.actionPerformed(event);
        }
    }
    
   
    private void openImage() {
        System.out.println("working9");

    	// if button is pressed without selection
    	if (tree.isSelectionEmpty())
    	{
    		MipavUtil.displayError("Nothing is selected!");
    		return;
    	}
    	
    	//get selected items
    	TreePath[] currentPath = tree.getSelectionPaths();
    	DefaultMutableTreeNode temp = null;

    	FileDicomTagTable currentImages[] = new FileDicomTagTable[currentPath.length];
    	for(int i = 0; i < currentPath.length; i++){
    		
    		temp = (DefaultMutableTreeNode) currentPath[i].getLastPathComponent();
    		
    		//make sure entire selection is comprised of images only
    		if (temp.toString() == "DICOMDIR")
    		{
				MipavUtil.displayError("Selection must be all images or a single series");
				return;    			
    		}
    		currentImages[i] = (FileDicomTagTable) temp.getUserObject();
    	}
    	
    	// if it is only a series, open all images in series
    	if (currentImages.length == 1){
    		String testIfSeries = currentImages[0].get("0004,1430").getValue(true).toString();
    		if (testIfSeries.startsWith("SERIES")){
    			currentImages = new FileDicomTagTable[temp.getChildCount()];
    			for (int i = 0; i < temp.getChildCount(); i++){
    				DefaultMutableTreeNode temp2 = (DefaultMutableTreeNode) temp.getChildAt(i);
    				currentImages[i] = (FileDicomTagTable) temp2.getUserObject();
    			}
    		}
    	}

    	//save image locations
		String[] fileNames = new String[currentImages.length];
        String fileFinalLoc = null;
        
    	for(int i = 0; i < currentImages.length;i++){

    		String currentItemType = currentImages[i].get("0004,1430").getValue(true).toString(); 
            
			if(currentItemType.startsWith("IMAGE"))
			{
    	    	String fileLoc = currentImages[i].get("0004,1500").getValue(true).toString();
    	    	fileLoc = fileLoc.replace('/', File.separatorChar);
    	    	fileLoc = fileLoc.replace('\\', File.separatorChar);
    	    	int loc = fileLoc.lastIndexOf(File.separatorChar);
    	    	if(loc != -1) {
    	    		String filename = fileLoc.substring(loc+1);
    	    		filename = filename.trim();
        	    	String filepreloc = fileLoc.substring(0, loc);
        	    	filepreloc = filepreloc.trim();
        	    	fileFinalLoc = file.getParent()+File.separator+filepreloc+File.separator;
        	    	fileNames[i] = filename;
    	    	}else {
    	    		String filename = fileLoc;
    	    		filename = filename.trim();
        	    	fileFinalLoc = file.getParent()+File.separator;
        	    	fileNames[i] = filename;
    	    	}

			
			} 
			else{
				MipavUtil.displayError("Selection must be all images or a single series");
				return;
			}
    	

    	}

    		//open files
            FileIO io = new FileIO();
            io.setQuiet(true);
            io.setFileDir(fileFinalLoc);

            ModelImage image = io.readDicom(fileNames[0], fileNames, false);

            if (image == null) {
                return;
            }

            image.calcMinMax();
            new ViewJFrameImage(image, io.getModelLUT());
		}
    		


	/**
     * Initializes the dialog box to a certain size and adds the components.
     *
     * @param  title  Title of the dialog box.
     */
    private void init(String title) {

        
        

        dirInfo = opener.getDirFileInfo();
        dicomInfo = (FileInfoDicom) opener.getFileInfo();
        
        Box scrollingBox = new Box(BoxLayout.Y_AXIS);

        buildBrightContPanel();
        
        setTitle(title);
        
        //build table
        String[] cols = new String[3];
        cols[0] = "Tag";
        cols[1] = "Name";
        cols[2] = "Value";
        tagViewer = new ViewTableModel((Object[])cols, 0);
        JTable tagTable = new JTable(tagViewer);
        JTableHeader header = tagTable.getTableHeader();
        header.setReorderingAllowed(false);
        scrollingBox.add(header);
        scrollingBox.add(tagTable);

 
        //build tree
        tagTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        JDialogFileInfoDICOM.showTagsNoCheckbox(tagViewer, dicomInfo, true);
        base = new DefaultMutableTreeNode("DICOMDIR");
        createNodes();
        tree = new JTreeDICOM(base);
        tree.addTreeSelectionListener(this);
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        JScrollPane treeView = new JScrollPane(tree);
        treeView.setPreferredSize(new Dimension(630,330));

        JPanel mainRootPanel = new JPanel(new BorderLayout());

        scrollPane = new JScrollPane(scrollingBox, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);


        
        JSplitPane mainPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, treeView, scrollPane);
        JSplitPane mainPanel2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, mainPanel, buildImagePanel());
        mainRootPanel.add(mainPanel2, BorderLayout.CENTER);
        
        JToolBar tBar = WidgetFactory.initToolbar();
        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);
        tBar.add(toolbarBuilder.buildButton(CustomUIBuilder.PARAM_DATA_DICOMDIR_OPEN_DICOMDIR));
        tBar.add(toolbarBuilder.buildButton(CustomUIBuilder.PARAM_DATA_DICOMDIR_OPEN_Images));

        
        buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(cancelButton);

        JMenu fileMenu = new JMenu("File");
        fileMenu.setFont(MipavUtil.font12B);

        JMenuItem itemExit = new JMenuItem("Exit");
        itemExit.addActionListener(this);
        itemExit.setActionCommand("Exit");
        itemExit.setFont(MipavUtil.font12B);
        fileMenu.add(itemExit);
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(fileMenu);
        setJMenuBar(menuBar);
        
        getContentPane().add(tBar, BorderLayout.NORTH);
        getContentPane().add(mainRootPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setSize(DEFAULT_SIZE);
        setVisible(true);
    }
    
	/**
     * Creates nodes on the tree based on DICOMDIR
     *
     */
    private void createNodes() {

    	    DefaultMutableTreeNode patient = null;
    	    DefaultMutableTreeNode study = null;
    	    DefaultMutableTreeNode series = null;
    	    DefaultMutableTreeNode image = null;
    	    
    	    for(int i = 0; i < dirInfo.getSequenceLength(); i++){
        	    String currentItemType = dirInfo.getItem(i).get("0004,1430").getValue(true).toString(); 
        	    if (currentItemType.startsWith("PATIENT"))
        	    {
        	    	patient = new DefaultMutableTreeNode(dirInfo.getItem(i));
        	    	base.add(patient);
        	    }
        	    else if(currentItemType.startsWith("STUDY"))
        	    {
        	    	study = new DefaultMutableTreeNode(dirInfo.getItem(i));
        	    	patient.add(study);
        	    }
        	    else if (currentItemType.startsWith("SERIES"))
        	    {
        	    	series = new DefaultMutableTreeNode(dirInfo.getItem(i));
        	    	study.add(series);
        	    }
        	    else if(currentItemType.startsWith("IMAGE"))
        	    {
        	    	image = new DefaultMutableTreeNode(dirInfo.getItem(i));
        	    	series.add(image);
        	    }
    	    }
    }


	/**
     * Open new DICOMDIR file, if successful then close current box
     */
    private void open() {
    	
    	JDialogDicomDir newer = new JDialogDicomDir(this.parentFrame);
    	if (newer.wasSuccess())
    		dispose();

    }
    

    
    /**
     * Opens preview of image in frame
     * 
     */
    protected FileInfoBase buildImage(String fileName, String directory) {

        FileIO io = new FileIO();
        FileInfoBase fileInfo = null;

        if (img != null) {
            imagePanel.remove(img);
            img.dispose(false);
            img = null;
        } else if (thumbnail != null) {
            imagePanel.remove(thumbnail);
            thumbnail.finalize();
        }


        io.setQuiet(true);

        ModelImage image = io.readOneImage(fileName, directory);

        if (image == null) {
            return null;
        }


        int[] extents = new int[] {image.getExtents()[0], image.getExtents()[1]};
        this.setPreferredSize(getPanelSize());
        img = new ViewJComponentPreviewImage(image, extents, this);
        img.createImg(0);
        brightness = brightSlider.getValue();
        contrast = (float) Math.pow(10.0, contSlider.getValue() / 200.0);
        
        img.setSliceBrightness(brightness, contrast);
        
        imagePanel.add(img);

        imagePanel.validate();
        imagePanel.repaint();

        fileInfo = (FileInfoBase) image.getFileInfo(0).clone();

        image.disposeLocal();
        image = null;

        return fileInfo;
    }
    /**
     * Initializes GUI components and displays dialog.
     * 
     * <p>
     * For the brightSlider the slider values and the brightness values are identical. brightness is an offset going
     * from -255 to 255. This is enough to change all 0 values to 255 and all 255 values to 0. brightness is added to
     * all contrast scaled red, green, and blue.
     * </p>
     * 
     * <p>
     * However, for the contrastSlider the slider values are different from the contrast values. The contrast values go
     * from 0.1 to 10.0 while the slider values go from -200 to 200. contrast =
     * (float)Math.pow(10.0,contSlider.getValue()/200.0) The original red, green, and blue are mutliplied by contrast.
     * </p>
     */
    private void buildBrightContPanel() {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        brightSlider = new JSlider(JSlider.HORIZONTAL, -255, 255, origBrightness);

        brightSlider.setMajorTickSpacing(102);
        brightSlider.setPaintTicks(true);
        brightSlider.setEnabled(true);
        brightSlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(255));

        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(origBrightness));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf( -255));

        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        sliderPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(brightSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Level"));

        contSlider = new JSlider(JSlider.HORIZONTAL, -200, 200,
                (int) (Math.round(86.85889638 * Math.log(origContrast))));

        contSlider.setMajorTickSpacing(80);
        contSlider.setPaintTicks(true);
        contSlider.setEnabled(true);
        contSlider.addChangeListener(this);

        JLabel maximum2 = new JLabel(String.valueOf(10));

        maximum2.setForeground(Color.black);
        maximum2.setFont(serif12);

        nfc = NumberFormat.getNumberInstance();
        nfc.setMaximumFractionDigits(3);

        current2 = new JLabel(String.valueOf(nfc.format(origContrast)));
        current2.setForeground(Color.black);
        current2.setFont(serif12B);

        JLabel minimum2 = new JLabel(String.valueOf(0.100));

        minimum2.setForeground(Color.black);
        minimum2.setFont(serif12);

        JPanel sliderPanel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel2.add(contSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel2.add(minimum2, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel2.add(current2, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(buildTitledBorder("Window"));

        JPanel centerPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.gridheight = 2;
        centerPanel.add(sliderPanel2, gbc2);

        gbc2.gridy = 2;
        centerPanel.add(sliderPanel, gbc2);

        gbc2.gridheight = 1;
        gbc2.gridy = 4;

        brightPanel = new JPanel(new BorderLayout());
        brightPanel.add(centerPanel);
        brightPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        brightPanel.setPreferredSize(new Dimension(190, 100));
        brightPanel.setMinimumSize(new Dimension(190, 100));


    }


	/**
     * Updates the table based on tree selection
     */
	public void valueChanged(TreeSelectionEvent e) {
		
		//Don't do anything if more than one item is selected
		if(tree.getSelectionCount()>1)
			return;
		
		if(!e.isAddedPath())
			return;
			
		DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();
		int howMany = tagViewer.getRowCount();
		
		for (int i = 0; i < howMany; i++)
			tagViewer.removeRow(0);
		
        if (img != null) {
            imagePanel.remove(img);
            img.dispose(false);
            img = null;
            imagePanel.validate();
            imagePanel.repaint();
        }
		
		if(!currentNode.toString().equals("DICOMDIR"))
		{
		    FileDicomTagTable currentObject = (FileDicomTagTable) currentNode.getUserObject();
    	    String currentItemType = currentObject.get("0004,1430").getValue(true).toString(); 
    	    if (currentItemType.startsWith("PATIENT"))
    	    {
    	    	JDialogFileInfoDICOM.showTags(tagViewer, currentObject, true);
    	    }
    	    else if(currentItemType.startsWith("STUDY"))
    	    {
    	    	JDialogFileInfoDICOM.showTags(tagViewer, currentObject, true);
    	    }
    	    else if (currentItemType.startsWith("SERIES"))
    	    {
    	    	JDialogFileInfoDICOM.showTags(tagViewer, currentObject, true);
    	    }
    	    else if(currentItemType.startsWith("IMAGE"))
    	    {
    	    	JDialogFileInfoDICOM.showTags(tagViewer, currentObject, true);
    	    	String fileLoc = currentObject.get("0004,1500").getValue(true).toString();
    	    	fileLoc = fileLoc.replace('/', File.separatorChar);
    	    	fileLoc = fileLoc.replace('\\', File.separatorChar);
    	    	int loc = fileLoc.lastIndexOf(File.separatorChar);
    	    	if(loc != -1) {
	    	    	String filename = fileLoc.substring(loc+1);
	    	    	filename = filename.trim();
	    	    	String filepreloc = fileLoc.substring(0, loc);
	    	    	filepreloc = filepreloc.trim();
	    	    	buildImage(filename,file.getParent()+File.separator+filepreloc+File.separator);
    	    	}else {
    	    		String filename = fileLoc;
    	    		filename = filename.trim();
    	    		buildImage(filename,file.getParent()+File.separator);
    	    	}
    	    	
    	    }
		} 
		else
			JDialogFileInfoDICOM.showTagsNoCheckbox(tagViewer, dicomInfo, true);
	
	}


	/**
     * Updates the image based on thumbnails
     */
	public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == brightSlider) {
            brightness = brightSlider.getValue();
            current.setText(String.valueOf(brightness));

            // if the image is not a thumbnail and it is larger than 1024 x 768,
            // do not adjust while sliding
            if ( (img != null) && (img.getImageSize() > 786432) && brightSlider.getValueIsAdjusting()) {
                return;
            }

            // Change only the brightness and contrast of the current slice
            if (img != null) {
                img.setSliceBrightness(brightness, contrast);
            } else if (thumbnail != null) {
                thumbnail.setBrightnessContrast(brightness, contrast);
            }
        } else if (source == contSlider) {
            contrast = (float) Math.pow(10.0, contSlider.getValue() / 200.0);
            current2.setText(String.valueOf(nfc.format(contrast)));

            // if the image is not a thumbnail and it is larger than 1024 x 768,
            // do not adjust while sliding
            if ( (img != null) && (img.getImageSize() > 786432) && contSlider.getValueIsAdjusting()) {
                return;
            }

            // Change only the brightness and contrast of the current slice
            if (img != null) {
                img.setSliceBrightness(brightness, contrast);
            } else if (thumbnail != null) {
                thumbnail.setBrightnessContrast(brightness, contrast);
            }

        }
    }


	public Dimension getPanelSize() {
		return new Dimension(imagePanel.getBounds().width, imagePanel.getBounds().height);
	}
	
	
    private JSplitPane buildImagePanel() {

        defaultImageSize = new Dimension(375,1000);
        imagePanel.setSize(defaultImageSize);

        brightPanel.setMaximumSize(new Dimension(200,200));
        imageSliderPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, imagePanel, brightPanel);
        imageSliderPane.setResizeWeight(.9);
        imageSliderPane.resetToPreferredSizes();
        return imageSliderPane;
    }
    
	public boolean wasSuccess() {

        return ifSuccess;
    }
}
