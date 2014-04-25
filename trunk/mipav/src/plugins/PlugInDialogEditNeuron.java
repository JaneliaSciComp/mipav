import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.PriorityQueue;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.EmptyBorder;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.JFrameHistogram;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;


public class PlugInDialogEditNeuron extends JDialogStandalonePlugin implements MouseListener, MouseMotionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2685558849611300514L;
	
	/**
	 * Slice in the subvolume that is considered the "center"
	 * which means which slice is editable
	 */
	private int activeSlice;

	private JRadioButton addRB;
	
	/**
	 * The active slice's actual depth is displayed here
	 */
	private JLabel centerLabel;
	
	/**
	 * VOI containing the points to edit the neuron traces
	 */
	private VOI controlPts;
	
	/**
	 * The active slice's actual depth in the full volume
	 */
	private int currentSlice;
	
	private JRadioButton deleteRB;
	
	private JCheckBox deleteBox;
	
	/**
	 * Depth of the current subvolume
	 */
	private int depth;
	
	private JTextField dirText;
	
	/**
	 * Whether or not a VOI is being dragged
	 */
	private boolean dragged;

	private JFileChooser fileChooser;
	
	private int height;
	
	private ArrayList<File> images;
	
	private int[] imBuffer;
	
	/**
	 * Used to keep track of which node to delete
	 */
	private VOIBase lastActive;
	
	private int length;
	
	/**
	 * List to keep track of which nodes are connected 
	 * in the active slice
	 */
	private Linking links;
	
	private ModelLUT lut;
	
	/**
	 * Neuron traces are overlayed on the subvolume
	 */
	private BitSet mask;
	
	private int numImages;
	
	/**
	 * Keeps track of the original start of the neuron
	 */
	private Point origin;
	
	/**
	 * Keeps track of the paths in the active slice so
	 * that they can be quickly moved/deleted/split
	 */
	private LineList paths;
	
	/**
	 * Which slice was the previous active slice. Should only
	 * be a difference of 1, but if an image list is implemented
	 * later, could be differet
	 */
	private int prevSlice;
	
	/**
	 * Whether or not a VOI was the target of the click
	 */
	private boolean ptClicked;
	
	private JCheckBox saveBox;
	
	/**
	 * The active slice within the subvolume is displayed here
	 */
	private JLabel sliceLabel;
	
	private int sliceRange;
	
	private JSpinner rangeField;
	
	/**
	 * Part of the 3D image to display on screen
	 */
	private ModelImage subVolume;
	
	/**
	 * The associated frame for the subvolume image
	 */
	private ViewJFrameImage subVolumeFrame;
	
	private ArrayList<File> swcList;
	
	/**
	 * Which node we are changing when a move action occurs
	 */
	private Point toChange;
	
	private int type;
	
	private int width;
	
	/**********************************************/
		
	private JRadioButton editPolyRB;
	
	private JRadioButton editTraceRB;
	
	private JRadioButton progenitorRB;
	
	private JRadioButton splitRB;
	
	private VOI polyVOI;
	
	private Point progenitorPt;
	
	private Point splitPt;
	
	private VOIPoint splitVOI;
	
	private VOIPoint progenitorVOI;
	
	private VOIPoint originVOI;
	
	private JRadioButton originRB;
	
	private JTextField xResField;
	
	@SuppressWarnings("rawtypes")
	private JComboBox resUnits;
	
	/**
	 * Default resolution right now, in um
	 */
	private double resolution = 0.211;
	
	private JCheckBox aviBox;
	
	private JRadioButton primaryRB;
	
	//private JRadioButton junctionRB;
	
	private VOIPoint primaryVOI;
	
	//private VOIPoint junctionVOI;
	
	private Point primaryPt;
	
	//private Point junctionPt;

	/**
	 * Primary constructor. Initializes a dialog to ask the user
	 * for a directory to use
	 */
	public PlugInDialogEditNeuron(){
		super();
		
		images = new ArrayList<File>();
		swcList = new ArrayList<File>();
		
		//Right now it's not changeable, but can be if needed
		//sliceRange = 5;
		currentSlice = 0;
		prevSlice = 0;
		
		init();
		
	}
	
	/**
	 * Secondary constructor used for when the neuron segmentation has
	 * ended and the user wishes to open up the editor immediately at the 
	 * conclusion of the first round of edits.
	 * @param directory the directory where images are (which accompanying 
	 * SWC file)
	 */
	public PlugInDialogEditNeuron(String directory){
		super();
		
		images = new ArrayList<File>();
		swcList = new ArrayList<File>();

		currentSlice = 0;
		prevSlice = 0;
		
		Preferences.setImageDirectory(new File(directory));
		init();
	}
	
	public void actionPerformed(ActionEvent event){
		String command = event.getActionCommand();
		
		if (command.equals("ApproveSelection")){
			dirText.setText(fileChooser.getSelectedFile().toString());
        	Preferences.setImageDirectory(fileChooser.getSelectedFile());
        }
		else if(command.equals("Cancel")){
			
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}
		else if(command.equals("Choose")) chooseDir();
		else if(command.equals("OK")){
			try{
				rangeField.commitEdit();
				Object val = rangeField.getValue();
				if(val instanceof Integer){
					sliceRange = (Integer)rangeField.getValue();
				} else {
					throw new NumberFormatException();
				}
			} catch (NumberFormatException e){
				MipavUtil.displayError("Range is not an integer");
				return;
			} catch (ParseException e) {
				e.printStackTrace();
			}
			populateImages(new File(dirText.getText()));
			initEditor();
        	openImage();	
		}
		else if(command.equals("Next")){
			prevSlice = currentSlice;
			currentSlice++;
			lut = subVolumeFrame.getLUTa();
			
        	if(currentSlice < numImages){
        		Point loc = subVolumeFrame.getLocation();
        		subVolumeFrame.close();
        		openImage();
        		subVolumeFrame.setLocation(loc);
        	} else {
        		actionPerformed(new ActionEvent(this, 0, "End"));
        	}
		} else if(command.equals("Prev")){
			if(currentSlice > 0 ){
				lut = subVolumeFrame.getLUTa();
				prevSlice = currentSlice;
				currentSlice--;
        		Point loc = subVolumeFrame.getLocation();
        		subVolumeFrame.close();
        		openImage();
        		subVolumeFrame.setLocation(loc);
			}
		}
		else if(command.equals("Reset")){
			subVolume.unregisterAllVOIs();
			try {
				readSWC(Math.max(0, currentSlice - sliceRange));
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		} else if(command.equals("LUT")){
			openHisto();
		} else if(command.equals("Save")){
			try{
				saveNewSWC();
			} catch(IOException e){
				e.printStackTrace();
			}
		} else if(command.equals("End")){
			if(aviBox.isSelected()){
				String dir = dirText.getText();
				if(!dir.endsWith(File.separator))
					dir += File.separator;
				dir += "Branch_Images" + File.separator;
				new PlugInDialogSaveTraceAsAVI(dir);
				subVolumeFrame.close();
				dispose();
			} else{
				if (isExitRequired()) {
		            System.exit(0);
		            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		        } else {
		        	subVolumeFrame.close();
		        	dispose();
		        }
			}
		} else if(command.equals("Zoom In")){
			float zX = subVolumeFrame.getComponentImage().getZoomX();
			float zY = subVolumeFrame.getComponentImage().getZoomY();
			if(zX <= 1){
				zX *= 2f;
				zY *= 2f;
			} else {
				zX += 1f;
				zY += 1f;
			}
			subVolumeFrame.getComponentImage().setZoom(zX, zY);
			subVolumeFrame.updateFrame(zX, zY);
			subVolumeFrame.updateImages();
		} else if(command.equals("Zoom Out")){
			float zX = subVolumeFrame.getComponentImage().getZoomX();
			float zY = subVolumeFrame.getComponentImage().getZoomY();
			if(zX <= 1){
				zX *= 0.5f;
				zY *= 0.5;
			} else {
				zX -= 1f;
				zY -= 1f;
			}
			subVolumeFrame.getComponentImage().setZoom(zX, zY);
			subVolumeFrame.updateFrame(zX, zY);
			subVolumeFrame.updateImages();
		} else if(command.equals("Edit Trace")){
			subVolume.unregisterAllVOIs();
			subVolume.registerVOI(controlPts);
			subVolume.notifyImageDisplayListeners();
			
			addRB.setEnabled(true);
			deleteRB.setEnabled(true);
			progenitorRB.setEnabled(true);
			splitRB.setEnabled(true);
			originRB.setEnabled(true);
			primaryRB.setEnabled(true);
			//Remove polygon, replace with control points
		} else if(command.equals("Edit Polygon")){
			if(polyVOI.getSize() == 0)
				polygonDisplay();
			subVolume.unregisterAllVOIs();
			subVolume.registerVOI(polyVOI);
			subVolume.notifyImageDisplayListeners();
			
			addRB.setEnabled(false);
			deleteRB.setEnabled(false);
			progenitorRB.setEnabled(false);
			splitRB.setEnabled(false);
			originRB.setEnabled(false);
			primaryRB.setEnabled(false);
			//Remove control points, replace with polygon
		}
	}
	
	/**
	 * Bresenham line algorithm used to draw the paths between two points.
	 * Uses the points taken from the SWC files to draw the paths in the 
	 * mask to overlay on the subvolume image. 
	 * @param x0
	 * @param y0
	 * @param x1
	 * @param y1
	 * @param z
	 */
	private void bresenham(int x0, int y0, int x1, int y1, int z){
		int dx = Math.abs(x1-x0);
		int dy = Math.abs(y1-y0);
		int sx, sy;
		int err, e2;
		if(x0 < x1)
			sx = 1;
		else sx = -1;
		if(y0 < y1)
			sy = 1;
		else sy = -1;
		err = dx - dy;
		
		int i;
		
		while(true){
			i = x0 + y0*width;
			mask.set(i+z*length);
			
			if(x0 == x1 && y0 == y1) break;
			e2 = 2*err;
			if(e2 > -dy){
				err -= dy;
				x0 += sx;
			}
			if(e2 < dx){
				err += dx;
				y0 += sy;
			}
		}
	}
	
	/**
	 * Bresenham line algorithm used to draw the paths between two points.
	 * This version is used with the subclasses to keep track of which
	 * lines are displayed in the active slice to make it easier to add
	 * nodes between points
	 * @param p0
	 * @param p1
	 * @return
	 */
	private ArrayList<Point> bresenham(Point p0, Point p1){
		
		int x0 = p0.x;
		int x1 = p1.x;
		int y0 = p0.y;
		int y1 = p1.y;
		
		ArrayList<Point> pts = new ArrayList<Point>();
		int dx = Math.abs(x1-x0);
		int dy = Math.abs(y1-y0);
		int sx, sy;
		int err, e2;
		if(x0 < x1)
			sx = 1;
		else sx = -1;
		if(y0 < y1)
			sy = 1;
		else sy = -1;
		err = dx - dy;
		
		int i;
		
		while(true){
			pts.add(new Point(x0, y0));
			i = x0 + y0*width;
			
			mask.set(i+activeSlice*length);

			if(x0 == x1 && y0 == y1) break;
			e2 = 2*err;
			if(e2 > -dy){
				err -= dy;
				x0 += sx;
			}
			if(e2 < dx){
				err += dx;
				y0 += sy;
			}
		}
		
		return pts;
	}
	
	/**
	 * Recursive function to traverse the entire linked list as a means
	 * for assigning branch tips a length and order. At each branch point, 
	 * the longest branch is allowed to continue while the shorter branch
	 * no longer is accounted for and thus has its order incremented. The
	 * increment occurs through the entire shorter branch, which requires
	 * an update of all child branches. 
	 * @param e
	 * @param prev
	 * @param length
	 * @return
	 * @throws IOException
	 */
	private PriorityQueue<NeuronLength> calcLengths(LinkElement e, LinkElement prev, double length) throws IOException{
		LinkElement l;
		PriorityQueue<NeuronLength> nList = new PriorityQueue<NeuronLength>();
		NeuronLength nl;
		
		if(e.linked.size()==1 && prev != null){
			nl = new NeuronLength(e.pt, length);
			nList.add(nl);
			return nList;
		} else if(e.linked.size()==2 && prev != null){
			l = e.linked.get(0) == prev ? e.linked.get(1) : e.linked.get(0);
			return calcLengths(l, e, length + e.pt.distance(l.pt));
		} else {
			ArrayList<LinkElement> list = e.copyList();
			PriorityQueue<NeuronLength> result = new PriorityQueue<NeuronLength>();
			ArrayList<PriorityQueue<NeuronLength>> listQ = new ArrayList<PriorityQueue<NeuronLength>>();
			if(prev != null)
				list.remove(prev);
			PriorityQueue<NeuronLength> pq = new PriorityQueue<NeuronLength>();
			for(int i=0;i<list.size();i++){
				l = list.get(i);
				result = calcLengths(l, e, e.pt.distance(l.pt));
				nList.addAll(result);
				listQ.add(result);
				pq.add(result.peek());
			}
			nl = pq.peek();

			nl.length += length;
			for(int i=0;i<list.size();i++){
				PriorityQueue<NeuronLength> temp = listQ.get(i);
				if(!temp.contains(nl)){
					Iterator<NeuronLength> iter = temp.iterator();
					while(iter.hasNext()){
						NeuronLength n = iter.next();
						n.order++;
					}
				}
			}
			
			return nList;
		}
	}
	
	private double[] calcPolyArea(){
		
		double[] output = new double[3];
		
		int xi, yi;
		
		BitSet polyMask = polyVOI.createBinaryMask(width, height, activeSlice);
		int sumX = 0;
		int sumY = 0;
		int area = 0;
		for (int i = polyMask.nextSetBit(0); i >= 0; i = polyMask.nextSetBit(i+1)){
			xi = i%width;
			yi = i/width;
			sumX += xi;
			sumY += yi;
			area++;
		}
		
		output[0] = (double)sumX/(double)area;
		output[1] = (double)sumY/(double)area;
		output[2] = area*Math.pow(resolution, 2);
		
		return output;
		
	}

	/**
	 * Opens the file chooser dialog to provide a directory in which
	 * to look for the images and SWC files
	 */
	private void chooseDir(){
		String dirText = Preferences.getImageDirectory();
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addActionListener(this);
		fileChooser.showOpenDialog(this);
	}
	
	private boolean dfsOrigin(LinkElement e, LinkElement prev){
		if(e.pt.equals(origin))
			return true;
		
		ArrayList<LinkElement> list = e.copyList();
		list.remove(prev);
		if(list.size() > 0){
			for(int i=0;i<list.size();i++){
				if(dfsOrigin(list.get(i), e))
					return true;
			}
		}
		return false;
	}
	
	/**
	 * Used with the primary constructor to allow the user to choose the
	 * directory where the images and SWC files are
	 */
	private void init(){
		setForeground(Color.black);
        setTitle("Neuron Segmentation: Edit SWC");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image File/Directory"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose a folder that contains neurons that have an<br>"
        		+ "accompanying SWC file</html>";

        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        dirText = new JTextField(30);
        dirText.setText(Preferences.getImageDirectory());
        dirText.setFont(serif12);
        choosePanel.add(dirText);
        
        JButton dirButton = new JButton("Choose");
        dirButton.setFont(serif12);
        dirButton.addActionListener(this);
        choosePanel.add(dirButton);
        
        JPanel rangePanel = new JPanel();
        rangePanel.setForeground(Color.black);
        
        JLabel rangeLabel = new JLabel("Slice Range: +/-");
        rangeLabel.setFont(serif12);
        rangePanel.add(rangeLabel);
        
        rangeField = new JSpinner(new SpinnerNumberModel(5, 1, 10, 1));
        rangeField.setFont(serif12);
        rangePanel.add(rangeField);
        
        PanelManager manage = new PanelManager();
        manage.add(choosePanel);
        manage.addOnNextLine(rangePanel);
        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);

        JPanel OKCancelPanel = new JPanel();

        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
	}
	
	/**
	 * The main dialog of this plugin, used to control the editor portions
	 * which include adding/deleting nodes.
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void initEditor(){
		
		getContentPane().removeAll();
		
		PanelManager manage = new PanelManager();
		ButtonGroup group = new ButtonGroup();
		
		FileIO imReader = new FileIO();
		ModelImage sliceIm = imReader.readImage(images.get(0).getAbsolutePath());
		int []extents = sliceIm.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		sliceIm.disposeLocal();
		
		JPanel descPanel = new JPanel();
		descPanel.setForeground(Color.black);
		descPanel.setBorder(buildTitledBorder("Editor Instructions"));
		
		String descStr = "<html><b>Choose add or delete. When adding nodes to the <br>"
				+ "trace, double click on a section between two connected<br>"
				+ "nodes. When deleting nodes, double click on a node to<br>"
				+ "remove it as a control point. To edit a node's location,<br>"
				+ "drag the node to a new location. The trace will update<br>"
				+ "once you release the mouse button.</b></html>";
		JLabel descLabel = new JLabel(descStr);
		descLabel.setFont(serif12);
		descPanel.add(descLabel);
		
		getContentPane().add(descPanel, BorderLayout.NORTH);
		
        
        JPanel editPanel = new JPanel(new GridLayout(0,2));
        editPanel.setForeground(Color.black);
        editPanel.setBorder(buildTitledBorder("Editor Mode"));
        
        editTraceRB = new JRadioButton("Edit Neuron Trace");
        editTraceRB.setFont(serif12);
        editTraceRB.setActionCommand("Edit Trace");
        editTraceRB.setSelected(true);
        editTraceRB.addActionListener(this);
        
        editPolyRB = new JRadioButton("Edit Polygon Area");
        editPolyRB.setFont(serif12);
        editPolyRB.setActionCommand("Edit Polygon");
        editPolyRB.addActionListener(this);

		manage.add(editPanel);
		
        ButtonGroup editGroup = new ButtonGroup();
        
        editGroup.add(editTraceRB);
        editGroup.add(editPolyRB);
        editPanel.add(editTraceRB);
        editPanel.add(editPolyRB);
		
		JPanel radioPanel = new JPanel(new GridLayout(0,2));
        radioPanel.setForeground(Color.black);
        radioPanel.setBorder(buildTitledBorder("Node Editor"));
        
        addRB = new JRadioButton("Add Node");
        addRB.setFont(serif12);
        addRB.setSelected(true);
        radioPanel.add(addRB);
        group.add(addRB);
        
        deleteRB = new JRadioButton("Delete Node");
        deleteRB.setFont(serif12);
        radioPanel.add(deleteRB);
        group.add(deleteRB);
        
        manage.addOnNextLine(radioPanel);
        
        //JPanel labelRBPanel = new JPanel();
        JPanel labelRBPanel = new JPanel(new GridLayout(0,2));
        labelRBPanel.setForeground(Color.black);
        labelRBPanel.setBorder(buildTitledBorder("Label Editor"));
        
        progenitorRB = new JRadioButton("Label Progenitor (P)");
        progenitorRB.setFont(serif12);
        labelRBPanel.add(progenitorRB);
        group.add(progenitorRB);
        
        splitRB = new JRadioButton("Label Split Point (S)");
        splitRB.setFont(serif12);
        labelRBPanel.add(splitRB);
        group.add(splitRB);
        
        originRB = new JRadioButton("Label Origin (O)");
        originRB.setFont(serif12);
        labelRBPanel.add(originRB);
        group.add(originRB);
        
        //JPanel extraPanel = new JPanel();
        //extraPanel.setForeground(Color.black);
        
        primaryRB = new JRadioButton("Label Primary Branch (1)");
        primaryRB.setFont(serif12);
        //extraPanel.add(primaryRB);
        labelRBPanel.add(primaryRB);
        group.add(primaryRB);
        
        /*junctionRB = new JRadioButton("Label Progenitor Junction (J)");
        junctionRB.setFont(serif12);
        //extraPanel.add(junctionRB);
        labelRBPanel.add(junctionRB);
        group.add(junctionRB);*/

        /*PanelManager subManager = new PanelManager();
        subManager.add(labelRBPanel);
        subManager.addOnNextLine(extraPanel);
        JPanel subPanel = subManager.getPanel();
        subPanel.setBorder(buildTitledBorder("Label options"));*/
        
		//manage.addOnNextLine(subPanel);
        manage.addOnNextLine(labelRBPanel);
        
		JPanel labelPanel = new JPanel();
		labelPanel.setForeground(Color.black);
		
		sliceLabel = new JLabel("Active Slice: 0");
		sliceLabel.setFont(serif12);
		sliceLabel.setBorder(new EmptyBorder(0,5,0,5));
		labelPanel.add(sliceLabel);
		
		
		//JPanel centerPanel = new JPanel();
		//centerPanel.setForeground(Color.black);
		
		centerLabel = new JLabel("Center Depth: 0");
		centerLabel.setFont(serif12);
		centerLabel.setBorder(new EmptyBorder(0,5,0,5));
		labelPanel.add(centerLabel);

		manage.addOnNextLine(labelPanel);
		
		JPanel resPanel = new JPanel();
        resPanel.setForeground(Color.black);
        resPanel.setBorder(buildTitledBorder("Image Resolutions"));
        
        JLabel xRes = new JLabel("Resolution: ");
        xRes.setForeground(Color.black);
        xRes.setFont(serif12);
        resPanel.add(xRes);
        
        xResField = new JTextField(5);
        xResField.setText(Double.toString(resolution));
        xResField.setHorizontalAlignment(JTextField.RIGHT);
        xResField.setFont(serif12);
        resPanel.add(xResField);

        Unit[] allSame = UnitType.getUnitsOfType(UnitType.LENGTH);
        int[] allSameMeasure = new int[allSame.length]; 
        for(int i=0; i<allSameMeasure.length; i++) {
            allSameMeasure[i] = allSame[i].getLegacyNum();
        }
        String[] unitArr = new String[allSameMeasure.length];
        for(int i=0; i<allSameMeasure.length; i++) {
        	Unit unit = Unit.getUnitFromLegacyNum(allSameMeasure[i]);
        	unitArr[i] = unit.getAbbrev();
        }
        resUnits = new JComboBox(unitArr);
        resUnits.setFont(serif12);
        resUnits.setSelectedItem("um");
        resPanel.add(resUnits);
		
		manage.addOnNextLine(resPanel);
		//manage.add(centerPanel);
		
		JPanel optionPanel = new JPanel(new GridLayout(0,3));
		optionPanel.setForeground(Color.black);
		optionPanel.setBorder(buildTitledBorder("Options"));
		
		saveBox = new JCheckBox("Save trace as image");
		saveBox.setFont(serif12);
		optionPanel.add(saveBox);
		
		deleteBox = new JCheckBox("Delete old SWC");
		deleteBox.setFont(serif12);
		optionPanel.add(deleteBox);
		
		aviBox = new JCheckBox("Save AVI");
		aviBox.setFont(serif12);
		optionPanel.add(aviBox);
		
		manage.addOnNextLine(optionPanel);
		
		JPanel boxPanel = new JPanel(new GridLayout(1,4));
        boxPanel.setForeground(Color.black);
        
        JButton zoomIn = new JButton("Zoom In");
        zoomIn.setFont(serif12);
        zoomIn.addActionListener(this);
        boxPanel.add(zoomIn);
        
        JButton zoomOut = new JButton("Zoom Out");
        zoomOut.setFont(serif12);
        zoomOut.addActionListener(this);
        boxPanel.add(zoomOut);
        
        JButton prevButton = new JButton("Prev");
        prevButton.setFont(serif12);
        prevButton.addActionListener(this);
        boxPanel.add(prevButton);
        
        JButton nextButton = new JButton("Next");
        nextButton.setFont(serif12);
        nextButton.addActionListener(this);
        boxPanel.add(nextButton);
        
        manage.addOnNextLine(boxPanel);
        
        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);
        
        JPanel buttonPanel = new JPanel(new GridLayout(0,4));
        buttonPanel.setForeground(Color.black);
        
        JButton resetButton = new JButton("Reset");
        resetButton.setFont(serif12);
        resetButton.addActionListener(this);
        buttonPanel.add(resetButton);
        
        JButton saveButton = new JButton("Save");
        saveButton.setFont(serif12);
        saveButton.addActionListener(this);
        buttonPanel.add(saveButton);
        
        JButton lutButton = new JButton("LUT");
        lutButton.setFont(serif12);
        lutButton.addActionListener(this);
        buttonPanel.add(lutButton);
        
        JButton endButton = new JButton("End");
        endButton.setFont(serif12);
        endButton.addActionListener(this);
        buttonPanel.add(endButton);
        
        
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        System.gc();
		
	}
	
	private void openHisto(){
		ViewJComponentEditImage comp = subVolumeFrame.getComponentImage();
		JFrameHistogram histoFrame = new JFrameHistogram(parentFrame, subVolume, null, comp.getLUTa(), null);
		histoFrame.histogramLUT(true, true);
	}
	
	/**
	 * Opens up the new subvolume. To keep read times for the images low, it will
	 * only load images as necessary. It will use previous portions of the images
	 * as you go up and down the stack.
	 */
	private void openImage(){
		
		int lowerBound = Math.max(0, currentSlice - sliceRange);
		int upperBound = Math.min(numImages - 1, currentSlice + sliceRange);
		depth = upperBound - lowerBound + 1;
		int[] sliceBuffer = new int[length];
		
		FileIO imReader = new FileIO();
		ModelImage sliceIm = null;
		
		try{
			if(currentSlice == prevSlice){//Initial case
				imBuffer = new int[length*depth];
				for(int i=0;i<depth;i++){
					sliceIm = imReader.readImage(images.get(i+lowerBound).getAbsolutePath());
					sliceIm.exportData(0, length, sliceBuffer);
					System.arraycopy(sliceBuffer, 0, imBuffer, i*length, length);
					type = sliceIm.getType();
					sliceIm.disposeLocal();
				}
			} else if(currentSlice < prevSlice){//Going backwards
				int[] tempBuffer = new int[length*depth];
				int prevLBound = Math.max(0, prevSlice - sliceRange);
				int copyDepth = upperBound - prevLBound + 1 ;
				int lBoundDiff = prevLBound - lowerBound;
				System.arraycopy(imBuffer, 0, tempBuffer, lBoundDiff*length, copyDepth*length);
				if(lBoundDiff != 0){
					sliceIm = imReader.readImage(images.get(lowerBound).getAbsolutePath());
					sliceIm.exportData(0, length, sliceBuffer);
					System.arraycopy(sliceBuffer, 0, tempBuffer, 0, length);
					sliceIm.disposeLocal();
				}
				imBuffer = tempBuffer;
			} else{//Going forwards
				int[] tempBuffer = new int[length*depth];
				int prevLBound = Math.max(0, prevSlice - sliceRange);
				int prevUBound = Math.min(numImages - 1, prevSlice + sliceRange);
				int copyDepth = prevUBound - lowerBound + 1;
				int lBoundDiff = lowerBound - prevLBound;
				int uBoundDiff = upperBound - prevUBound;
				System.arraycopy(imBuffer, lBoundDiff*length, tempBuffer, 0, copyDepth*length);
				if(uBoundDiff != 0){
					sliceIm = imReader.readImage(images.get(upperBound).getAbsolutePath());
					sliceIm.exportData(0, length, sliceBuffer);
					System.arraycopy(sliceBuffer, 0, tempBuffer, copyDepth*length, length);
					sliceIm.disposeLocal();
				}
				imBuffer = tempBuffer;
			}
			
			activeSlice = currentSlice - lowerBound;

			String label = sliceLabel.getText();
			String[] parts = label.split(" ");
			String sliceStr = String.valueOf(currentSlice);
			parts[2] = String.valueOf(activeSlice);
			sliceLabel.setText(parts[0] + " " + parts[1] + " " + parts[2]);
			
			label = centerLabel.getText();
			parts = label.split(" ");
			
			centerLabel.setText(parts[0] + " " + parts[1] + " " + sliceStr);
			
			subVolume = new ModelImage(type, new int[]{width, height, depth}, "Sub-Volume");
			subVolume.importData(0, imBuffer, true);
			subVolumeFrame = new ViewJFrameImage(subVolume, lut);
			subVolumeFrame.setSlice(activeSlice);
			subVolumeFrame.setVisible(true);
			subVolumeFrame.addWindowListener(this);
			
				
		
		} catch(IOException e){
			e.printStackTrace();
		}

		try {
			readSWC(lowerBound);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		editTraceRB.setSelected(true);
		actionPerformed(new ActionEvent(this, 0, "Edit Trace"));

		
		/**
		 * Useful for if you choose to implement an image list to jump around in the image
		 */
		/*if(Math.abs(sliceDiff) >= sliceRange){
			imBuffer = new int[length*depth];
			for(int i=0;i<depth;i++){
				sliceIm = imReader.readImage(images.get(i+lowerBound).getAbsolutePath());
				sliceIm.exportData(0, length, sliceBuffer);
				System.arraycopy(sliceBuffer, 0, imBuffer, i*length, length);
			}
		}
		else{
			int prevLBound = Math.max(0, prevSlice - sliceRange);
			int prevUBound = Math.min(numImages - 1, prevSlice + sliceRange);
			if(sliceDiff > 0){
				for(int i=0;i<=prevUBound-lowerBound;i++){
					
				}
			} else{
				for(int i=0;i<=upperBound-prevLBound;i++){
					
				}
			}
		}*/
		
	}
	
	/**
	 * Unlike the openImage method, the SWC files take far less time to read 
	 * so you can just read them all every single time. This also takes care
	 * of when some SWCs change due to editing, so that will show up in later 
	 * subvolumes. 
	 * 
	 * Reads the SWC file and draws the lines on the displayed paint mask. 
	 * Point VOIs are also used to control the editing. 
	 * @param lowerBound
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	private void readSWC(int lowerBound) throws FileNotFoundException, IOException{
		
		//Reset things regarding the mask/control points here
		mask = new BitSet(length*depth);
		controlPts = new VOI((short) 0, "Control Points", VOI.POINT, 0);
		paths = new LineList();
		links = new Linking();
		progenitorPt = null;
		splitPt = null;
		primaryPt = null;
		//junctionPt = null;
		progenitorVOI = null;
		splitVOI = null;
		primaryVOI = null;
		//junctionVOI = null;
		polyVOI = new VOI((short)1, "Polygon Area", VOI.CONTOUR, -1);
		
		ArrayList<String[]> points = null;
		ArrayList<Point> polyPt = new ArrayList<Point>();
		String[] lineArray;
		
		for(int k=0;k<depth;k++){
			BufferedReader input = new BufferedReader(new FileReader(swcList.get(k + lowerBound)));
			String line = null; 
			points = new ArrayList<String[]>();
			while (( line = input.readLine()) != null){
				if(k==activeSlice){
					if(line.startsWith("# Progenitor")){
						line = line.substring(line.indexOf("(") + 1);
						int ind = line.indexOf(",");
						int xp = Integer.valueOf(line.substring(0, ind));
						line = line.substring(ind+1);
						ind = line.indexOf(")");
						int yp = Integer.valueOf(line.substring(0, ind));
						progenitorPt = new Point(xp, yp);
						continue;
					} else if(line.startsWith("# Split")){
						line = line.substring(line.indexOf("(") + 1);
						int ind = line.indexOf(",");
						int xs = Integer.valueOf(line.substring(0, ind));
						line = line.substring(ind+1);
						ind = line.indexOf(")");
						int ys = Integer.valueOf(line.substring(0, ind));
						splitPt = new Point(xs, ys);
						continue;
					} else if(line.startsWith("# Poly Pt")){
						line = line.substring(line.indexOf("(") + 1);
						int ind = line.indexOf(",");
						int xs = Integer.valueOf(line.substring(0, ind));
						line = line.substring(ind+1);
						ind = line.indexOf(")");
						int ys = Integer.valueOf(line.substring(0, ind));
						Point pt = new Point(xs, ys);
						polyPt.add(pt);
						continue;
					} else if(line.startsWith("# Primary")){
						line = line.substring(line.indexOf("(") + 1);
						int ind = line.indexOf(",");
						int xs = Integer.valueOf(line.substring(0, ind));
						line = line.substring(ind+1);
						ind = line.indexOf(")");
						int ys = Integer.valueOf(line.substring(0, ind));
						Point pt = new Point(xs, ys);
						primaryPt = pt;
						continue;
					} /*else if(line.startsWith("# Junction")){
						line = line.substring(line.indexOf("(") + 1);
						int ind = line.indexOf(",");
						int xs = Integer.valueOf(line.substring(0, ind));
						line = line.substring(ind+1);
						ind = line.indexOf(")");
						int ys = Integer.valueOf(line.substring(0, ind));
						Point pt = new Point(xs, ys);
						junctionPt = pt;
						continue;
					}*/
				}
				if(line.startsWith("#"))
					continue;
				lineArray = line.split(" ");
				points.add(lineArray);
			}
			input.close();
			
			if(polyPt.size()>0 && k == activeSlice){
				Vector3f[] tipVec = new Vector3f[polyPt.size()];
				for(int i=0;i<polyPt.size();i++){
					Point pt = polyPt.get(i);
					tipVec[i] = new Vector3f(pt.x, pt.y, activeSlice);
				}
				VOIContour curve = new VOIContour(true);
				curve.importPoints(tipVec);
				
				polyVOI.importCurve(curve);
			}
			
			int linkedTo;
			int x0, x1, y0, y1;
			Point p0, p1;
			String num;
			LinkElement e0, e1;
			
			lineArray = points.get(0);
			num = lineArray[2];
			num = num.substring(0, num.indexOf("."));
			x0 = Integer.parseInt(num);
			num = lineArray[3];
			num = num.substring(0, num.indexOf("."));
			y0 = height - Integer.parseInt(num);
			
			if(k == activeSlice){
				origin = new Point(x0,y0);
				VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0,y0,activeSlice));
				voi.setLabel("O");
				controlPts.importCurve(voi);
				
				VOIBaseVector pts = controlPts.getCurves();
				originVOI = (VOIPoint) pts.get(0);
			}
			
			for(int i=points.size()-1;i>0;i--){
				lineArray = points.get(i);
				num = lineArray[2];
				num = num.substring(0, num.indexOf("."));
				x0 = Integer.parseInt(num);
				num = lineArray[3];
				num = num.substring(0, num.indexOf("."));
				y0 = height - Integer.parseInt(num);
				
				//Make the various line VOIs
				linkedTo = Integer.parseInt(lineArray[6]);

				lineArray = points.get(linkedTo - 1);
				num = lineArray[2];
				num = num.substring(0, num.indexOf("."));
				x1 = Integer.parseInt(num);
				num = lineArray[3];
				num = num.substring(0, num.indexOf("."));
				y1 = height - Integer.parseInt(num);
				
				if(k != activeSlice){
					bresenham(x0, y0, x1, y1, k);
				} else {
					p0 = new Point(x0, y0);
					p1 = new Point(x1, y1);
					e0 = links.get(p0);
					e1 = links.get(p1);
					
					if(e0 == null){
						e0 = new LinkElement(p0);
					}
					if(e1 == null){
						e1 = new LinkElement(p1);
					}
					e0.addLinkTo(e1);
					
					
					paths.add(p0, p1);
					
					VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0,y0,activeSlice));
					voi.setLabel("");
					controlPts.importCurve(voi);
				}
			}
		}
		
		VOIBaseVector base = controlPts.getCurves();
		VOIPoint ptVOI;
		Vector3f ptVec;
		for(int i=0;i<base.size();i++){
			ptVOI = (VOIPoint)base.get(i);
			ptVec = ptVOI.exportPoint();
			Point checkPt = new Point((int)ptVec.X, (int)ptVec.Y);
			if(splitPt != null && splitPt.equals(checkPt)){
				splitVOI = ptVOI;
				splitVOI.setLabel("S");
			} else if(progenitorPt != null && progenitorPt.equals(checkPt)) {
				progenitorVOI = ptVOI;
				progenitorVOI.setLabel("P");
			} else if(primaryPt != null && primaryPt.equals(checkPt)){
				primaryVOI = ptVOI;
				primaryVOI.setLabel("1");
			} /*else if(junctionPt != null && junctionPt.equals(checkPt)){
				junctionVOI = ptVOI;
				junctionVOI.setLabel("J");
			}*/
		}
		
		subVolume.getVOIs().add(controlPts);
		subVolume.notifyImageDisplayListeners(null, true, 0, -1);
		
		subVolumeFrame.getComponentImage().setPaintMask(mask);
		subVolumeFrame.getComponentImage().addMouseListener(this);
		subVolumeFrame.getComponentImage().addMouseMotionListener(this);
		subVolumeFrame.getControls().getTools().setOpacity(1.0f);
		subVolumeFrame.getControls().getTools().setPaintColor(Color.GREEN);
		subVolumeFrame.updateImages();
		
	}
	
	/**
	 * Saves the new neuron trace into the old SWC file while moving the
	 * old file into a new file denoted by "_old". 
	 * @throws IOException
	 */
	private void saveNewSWC() throws IOException{
		
		try{
			resolution = Double.valueOf(xResField.getText());
		} catch(NumberFormatException n){
			MipavUtil.displayError("Resolution must be a number");
			return;
		}
		
		if(originVOI == null){
			MipavUtil.displayError("No origin is set. Please choose an origin point.");
			return;
		}
		
		int counter = 0;
		File currentSWC = swcList.get(currentSlice);
		
		String header = "";
		
		String saveName = currentSWC.getPath();
		String name = currentSWC.getName();
		String parent = currentSWC.getParent();
		
		if(deleteBox.isSelected()){
			currentSWC.delete();
		} else{
			//Rename old files by appending .# to them
			//Higher number means more recent
			File oldSWC;
			do{
				String newName = name + "." + counter;
				oldSWC = new File(parent + File.separator + newName);
				counter++;
			}while(oldSWC.exists());
			
			currentSWC.renameTo(oldSWC);
		}
		currentSWC = new File(saveName);
		FileWriter writer = new FileWriter(currentSWC);
		
		if(polyVOI.getSize() == 0)
			polygonDisplay();
		
		double[] areaResults = calcPolyArea();
		header += String.format("# Polygonal Area: %5.2f %s\n", areaResults[2], (String)resUnits.getSelectedItem());
		header += String.format("# Centroid: (%4.2f,%4.2f)\n", areaResults[0], areaResults[1]);
		header += String.format("# Origin (%d,%d)\n", origin.x, origin.y);
		if(progenitorPt != null)
			header += String.format("# Progenitor Point (%d,%d)\n", progenitorPt.x, progenitorPt.y);
		if(splitPt != null)
			header += String.format("# Split Point (%d,%d)\n", splitPt.x, splitPt.y);
		if(primaryPt != null)
			header += String.format("# Primary Branch Point (%d,%d)\n", primaryPt.x, primaryPt.y);
		/*if(junctionPt != null)
			header += String.format("# Junction Point (%d,%d)\n", junctionPt.x, junctionPt.y);*/
		
		VOIBase base = polyVOI.getCurves().get(0);
		
		int[] xBuffer = new int[base.size()];
		int[] yBuffer = new int[base.size()];
		int[] zBuffer = new int[base.size()];
		base.exportArrays(xBuffer, yBuffer, zBuffer);
		
		for(int i=0;i<base.size();i++){
			header += String.format("# Poly Pt (%d,%d)\n", xBuffer[i], yBuffer[i]);
		}
		
		writer.append(header);

		if(saveBox.isSelected()){
			BitSet tempMask = new BitSet(length);
			for(int i=mask.nextSetBit(activeSlice*length); i>=0 && i<(activeSlice+1)*length; i = mask.nextSetBit(i+1)){
				tempMask.set(i-activeSlice*length);
			}
			
			ModelImage trace = new ModelImage(ModelImage.BOOLEAN, new int[]{width, height}, "Skel");
			trace.importData(0, tempMask, true);
			File current = images.get(currentSlice);
			name = current.getName();
			int splitAt = name.lastIndexOf(".");
			String fileName = name.substring(0, splitAt) + "_trace";
			
			trace.saveImage(parent + File.separator, fileName, FileUtility.TIFF, true);
			trace.disposeLocal();
		}
		
		int line = 1;
		LinkElement start = links.get(origin);
		String lengthHeader = "# Coordinates are for endpoints of projections\n"
				+ "# Distances are to branch point of this projection's order\n";
		/*if(links.size()==2){
			LinkElement e = links.get(0) == start ? links.get(1) : start;
			writer.append(lengthHeader);
			writePoint(new NeuronLength(e.pt, e.pt.distance(start.pt)), writer);
		} else if(links.size() > 2){*/
		PriorityQueue<NeuronLength> pq = new PriorityQueue<NeuronLength>();
		writer.append(lengthHeader);
		if(splitPt != null){
			LinkElement split = links.get(splitPt);
			LinkElement toOrigin = null;
			ArrayList<LinkElement> list = split.linked;
			for(int i=0;i<list.size();i++){
				LinkElement next = list.get(i);
				if(dfsOrigin(next, split)){
					toOrigin = next;
				}
			}
			//Find lenghts towards the origin: 
			writer.append("# Lengths towards origin\n");
			pq.addAll(calcLengths(toOrigin, split, toOrigin.pt.distance(split.pt)));
			NeuronLength nl;
			while((nl = pq.poll()) != null)
				writePoint(nl, writer);
			writer.append("# Lengths towards progenitor\n");
			pq.clear();
			pq.addAll(calcLengths(split, toOrigin, 0));
			while((nl = pq.poll()) != null)
				writePoint(nl, writer);
			/*LinkElement split = links.get(splitPt);
				LinkElement e = split.linked.get(0);
				pq.addAll(calcLengths(e, split, e.pt.distance(split.pt)));
				e = split.linked.get(1);
				pq.addAll(calcLengths(e, split, e.pt.distance(split.pt)));*/
		} else{
			//LinkElement e = start.linked.get(0);
			pq = calcLengths(start, null, 0);
			NeuronLength nl;
			while((nl = pq.poll()) != null)
				writePoint(nl, writer);
		}
			
			
		//}
		
		String dimStr = String.format("# Width %d\n# Height %d\n", width, height);
		String noteStr = 
				  "########################################################\n"
				+ "#              START OF SWC COORDINATES                #\n"
				+ "########################################################\n"
				+ "# NOTE: The above coordinates are in image space coordinates\n"
				+ "# Y-coordinates for SWC format are inverted in image space.\n"
				+ "# Image Y-coordinate = Image Height - SWC Y-coordinate\n";
		writer.append(dimStr);
		writer.append(noteStr);
		
		Point pt = start.pt;
		int[] parts = new int[7];

		parts[0] = line;
		parts[1] = 2;
		parts[2] = pt.x;
		parts[3] = height - pt.y;
		parts[4] = 0;
		parts[5] = 1;
		parts[6] = -1;
		
		String outString = String.valueOf(parts[0]) + " ";
		outString += String.valueOf(parts[1]) + " ";
		outString += String.valueOf(parts[2]) + ".0 ";
		outString += String.valueOf(parts[3]) + ".0 ";
		outString += String.valueOf(parts[4]) + ".0 ";
		outString += String.valueOf(parts[5]) + ".0 ";
		outString += String.valueOf(parts[6]) + "\n";
		
		writer.append(outString);
		
		ArrayDeque<Integer> parentLine = new ArrayDeque<Integer>();
		ArrayDeque<LinkElement> parentPt = new ArrayDeque<LinkElement>();
		
		HashSet<LinkElement> visited = new HashSet<LinkElement>();
		visited.add(start);
		LinkElement next;
		
		ArrayList<LinkElement> linkedTo = start.linked;
		for(int i=0;i<linkedTo.size();i++){
			next = linkedTo.get(i);
			parentLine.addFirst(line);
			parentPt.addFirst(next);
			visited.add(next);
		}
		
		while(parentPt.peek() != null){
			line++;
			start = parentPt.pollFirst();
			pt = start.pt;
			linkedTo = start.linked;
			parts[0] = line;
			if(linkedTo.size() == 1)
				parts[1] = 6;
			else if(linkedTo.size() == 2)
				parts[1] = 7;
			else parts[1] = 5;
			parts[2] = pt.x;
			parts[3] = height - pt.y;
			parts[4] = 0;
			parts[5] = 1;
			parts[6] = parentLine.pollFirst();
			
			outString = String.valueOf(parts[0]) + " ";
			outString += String.valueOf(parts[1]) + " ";
			outString += String.valueOf(parts[2]) + ".0 ";
			outString += String.valueOf(parts[3]) + ".0 ";
			outString += String.valueOf(parts[4]) + ".0 ";
			outString += String.valueOf(parts[5]) + ".0 ";
			outString += String.valueOf(parts[6]) + "\n";
			
			writer.append(outString);
			
			for(int i=0;i<linkedTo.size();i++){
				next = linkedTo.get(i);
				if(visited.add(next)){
					parentLine.addFirst(line);
					parentPt.addFirst(next);
				}
			}
		}
		
		writer.close();
	}
	
	private void writePoint(NeuronLength n, FileWriter writer) throws IOException{
		Point pt = n.endPt;
		double length = resolution * n.length;
		int o = n.order;
		String coord = String.format("# (%d, %d) ", pt.x, pt.y);
		//String order = "";
		String order = String.format("Branch order: %d ", o);
		String value = String.format("Length: %4.2f %s", length, (String)resUnits.getSelectedItem());
		String outString = coord + order + value + "\n";
		writer.append(outString);
				
	}
	
	private void polygonDisplay(){
		
		ArrayList<Point> polyPt = new ArrayList<Point>();
		polyVOI = new VOI((short)1, "Polygon Area", VOI.CONTOUR, -1);
		for(int i=0;i<links.size();i++){
			LinkElement l = links.get(i);
			if(l.linked.size() == 1){
				polyPt.add(l.pt);
			}
		}
		
		
		Vector3f[] tipVec = new Vector3f[polyPt.size()];
		for(int i=0;i<polyPt.size();i++){
			Point pt = polyPt.get(i);
			tipVec[i] = new Vector3f(pt.x, pt.y, activeSlice);
		}
		VOIContour curve = new VOIContour(true);
		curve.importPoints(tipVec);
		curve.convexHull();
		
		polyVOI.importCurve(curve);
		
	}
	
	/**
	 * Method used to determine which files in the chosen
	 * directory to add, which are then added to the
	 * list <code>images</code>. A filename filter is used
	 * to choose images with filetype tif or lsm. This 
	 * method also works recursively to add candidate images
	 * in folder within the main directory.
	 * 
	 * @param dir the directory to search through for files
	 * @return true if the list is non-empty
	 */
	
	private void populateImages(File dir){
		
		File skelName;
		String stripped;
		
		FilenameFilter imFilter = new FilenameFilter(){
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff")
						|| name.toLowerCase().endsWith(".lsm"));
			}
		};
		File[] files = dir.listFiles(imFilter);
		String dirStr = dir.toString();
		if(!dirStr.endsWith(File.separator))
			dirStr = dirStr + File.separator;
		dirStr = dirStr.concat("Branch_Images" + File.separator);
		
		for(File im : files){
			
			stripped = im.getName();
			stripped = stripped.substring(0, stripped.indexOf("."));
			stripped = stripped.concat("_branches.swc");
			skelName = new File(dirStr.concat(stripped));
			if(skelName.exists()){
				images.add(im);
				swcList.add(skelName);
			}
		}
		
		File[] directories = dir.listFiles(new FileFilter() {
			public boolean accept(File path) {
				return (path.isDirectory() && !path.getName().equals("Branch_Images"));
			}
		});
		
		for(int i=0;i<directories.length;i++){
			populateImages(directories[i]);
		}
		numImages = images.size();
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		if(editPolyRB.isSelected())
			return;
		
		if(ptClicked)
			dragged = true;
			
	}

	@Override
	public void mouseMoved(MouseEvent e) {

		
	}

	/**
	 * Checks for double clicks of the left mouse. If a double click
	 * is detected, a node is either added or deleted from the editable
	 * neuron trace (depending on which radio button is selected).
	 */
	@Override
	public void mouseClicked(MouseEvent e) {
		
		if(editPolyRB.isSelected())
			return;
		
		int numClicks = e.getClickCount();
		int button = e.getButton();
		
		if(numClicks != 2 || button != MouseEvent.BUTTON1)
			return;
		
		float zoomX = subVolumeFrame.getComponentImage().getZoomX();
		float zoomY = subVolumeFrame.getComponentImage().getZoomY();
		int x = (int) ((float)e.getX()/zoomX); //- left;
		int y = (int) ((float)e.getY()/zoomY); //- top;
		
		
		if(addRB.isSelected()){
			
			int ind = -1;
			
			if(mask.get(x + y*width)){
				ind = 0;
			}
			else{
				for(int ny = y-1;ny<=y+1;ny++){
					if(ny < 0 || ny >= height) continue;
					for(int nx = x-1;nx<=x+1;nx++){
						if(nx < 0 || nx >= width) continue;
						ind = nx + ny*width;
						if(mask.get(ind)){ 
							x = nx;
							y = ny;
							break;
						}
					}
				}
			}
			
			if(ind == -1) return;

			LinePath onPath = paths.findPath(new Point(x, y));
			if(onPath == null)
				System.out.println("Did not find path");
			else{
				Point pt = new Point(x,y);
				links.addNode(pt, onPath);
			}

		} else if (deleteRB.isSelected()){
			//search list of VOIs for this point
			VOIBase activeVOI = lastActive;

			if(activeVOI != null){
				controlPts.removeCurve(activeVOI);
				
			} else return;
			
			VOIPoint ptVOI = (VOIPoint) activeVOI;
			Vector3f ptVec = ptVOI.exportPoint();
			Point coord = new Point((int)ptVec.X, (int)ptVec.Y);
			if(coord.equals(origin)){
				LinkElement elem = links.get(origin);
				origin = elem.linked.get(0).pt;
				VOIBaseVector base = controlPts.getCurves();
				for(int i=0;i<base.size();i++){
					ptVOI = (VOIPoint)base.get(i);
					ptVec = ptVOI.exportPoint();
					Point checkPt = new Point((int)ptVec.X, (int)ptVec.Y);
					if(origin.equals(checkPt)){
						originVOI = ptVOI;
						originVOI.setLabel("O");
						subVolume.notifyImageDisplayListeners();
						break;
					}
				}
				//need to change origin VOI
			}
				
			links.removeNode(coord);

			lastActive = null;
			
			subVolume.notifyImageDisplayListeners();

		} else if(progenitorRB.isSelected() || splitRB.isSelected() || originRB.isSelected()
				|| primaryRB.isSelected() /*|| junctionRB.isSelected()*/){
			VOIBase activeVOI = lastActive;
			if(activeVOI == null) return;
			VOIPoint ptVOI = (VOIPoint) activeVOI;
			Vector3f ptVec = ptVOI.exportPoint();
			Point pt = new Point((int)ptVec.X, (int)ptVec.Y);
			if(activeVOI == progenitorVOI){
				progenitorVOI.setLabel("");
				progenitorVOI = null;
				progenitorPt = null;
			} else if(activeVOI == splitVOI){
				splitVOI.setLabel("");
				splitVOI = null;
				splitPt = null;
			} else if(activeVOI == originVOI){
				originVOI.setLabel("");
				originVOI = null;
				origin = null;
			} else if(activeVOI == primaryVOI){
				primaryVOI.setLabel("");
				primaryVOI = null;
				primaryPt = null;
			} /*else if(activeVOI == junctionVOI){
				junctionVOI.setLabel("");
				junctionVOI = null;
				junctionPt = null;
			}*/
			if(progenitorRB.isSelected()){
				if(progenitorVOI != null)
					progenitorVOI.setLabel("");
				progenitorPt = pt;
				progenitorVOI = ptVOI;
				progenitorVOI.setLabel("P");
			} else if(splitRB.isSelected()){
				if(splitVOI != null)
					splitVOI.setLabel("");
				splitPt = pt;
				splitVOI = ptVOI;
				splitVOI.setLabel("S");
			} else if(originRB.isSelected()){
				if(originVOI != null)
					originVOI.setLabel("");
				origin = pt;
				originVOI = ptVOI;
				originVOI.setLabel("O");
			} else if(primaryRB.isSelected()){
				if(primaryVOI != null)
					primaryVOI.setLabel("");
				primaryPt = pt;
				primaryVOI = ptVOI;
				primaryVOI.setLabel("1");
			} /*else if(junctionRB.isSelected()){
				if(junctionVOI!= null)
					junctionVOI.setLabel("");
				junctionPt = pt;
				junctionVOI = ptVOI;
				junctionVOI.setLabel("J");
			}*/
			
			subVolume.notifyImageDisplayListeners();
		} 
		
		controlPts.setAllActive(false);
		ptClicked = false;

		
	}

	/**
	 * The precursor to a drag movement involves a button
	 * press, so check to see if a VOI was clicked. If not,
	 * the any following move or delete actions will not
	 * occur. 
	 */
	@Override
	public void mousePressed(MouseEvent e) {

		if(editPolyRB.isSelected())
			return;
		
		VOIBaseVector pts = controlPts.getCurves();
		VOIBase activeVOI = null;
		
		for(int i=0;i<pts.size();i++){
			VOIBase current = pts.get(i);
			if(current.isActive()){
				activeVOI = current;
			}
		}
		
		lastActive = activeVOI;
		
		if(activeVOI != null){
			ptClicked = true;
			VOIPoint ptVOI = (VOIPoint) activeVOI;
			Vector3f vPt = ptVOI.getPosition();
			toChange = new Point((int)vPt.X, (int)vPt.Y);
		}
	}

	/**
	 * Once the mouse is released, check to see if it was preceeded
	 * by a drag event, in which case you need to move whichever node
	 * was just dragged to another spot.
	 */
	@Override
	public void mouseReleased(MouseEvent e) {
		
		if(editPolyRB.isSelected()){
			polyVOI = subVolume.getVOIs().get(0);
			return;
		}

		ptClicked = false;
		//update position of the control points in the line structures if the mouse was dragged
		//which implies the VOI was moved (also need to check if the VOI was selected);
		if(dragged){
			dragged = false;
			
			Vector3f ptVec = ((VOIPoint)lastActive).exportPoint();
			int x = (int) ptVec.X;
			int y = (int) ptVec.Y;
			Point pt = new Point(x,y);
			
			if(toChange.equals(origin))
				origin = pt;
			else if(toChange.equals(progenitorPt))
				progenitorPt = pt;
			else if(toChange.equals(splitPt))
				splitPt = pt;
			
			//We know that the drag happened on a VOI, so now update all links and lines
			
			links.moveNode(toChange, pt);
			
			lastActive = null;
			toChange = null;
		}
		
		controlPts.setAllActive(false);
		subVolume.notifyImageDisplayListeners();
		subVolumeFrame.updateImages();
		
		
	}

	@Override
	public void mouseEntered(MouseEvent e) {

	}

	@Override
	public void mouseExited(MouseEvent e) {
		
	}
	
	public void windowClosing(WindowEvent e){
		if(e.getSource() == subVolumeFrame){
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}
	}
	
	/**
	 * Subclass list used to keep track of which lines are displayed
	 * in the active slice. This is mainly used to find the endpoints
	 * of the line when a node is added on a line so that connections
	 * can be properly updated.
	 * 
	 * Implements an ArrayList, but adds a couple of methods that are
	 * specific to finding elements based on coordinates rather than
	 * pointers to objects.
	 * @author wangvg
	 *
	 */
	private class LineList extends ArrayList<LinePath>{
		
		/**
		 * 
		 */
		private static final long serialVersionUID = 6977450762745726017L;
	
		private LineList(){
			super();
		}
		
		/**
		 * Add a new line to the list/mask based on the
		 * two endpoints.
		 * @param p1
		 * @param p2
		 */
		private void add(Point p1, Point p2){
			ArrayList<Point> list = bresenham(p1,p2);
			this.add(new LinePath(p1, p2, list));
		}
		
		/**
		 * Remove the line that has these two endpoints from
		 * the list
		 * @param p1
		 * @param p2
		 */
		private void remove(Point p1, Point p2){
			for(int i=0;i<size();i++){
				LinePath p = get(i);
				if((p1.equals(p.pt1) && p2.equals(p.pt2)) || 
						(p2.equals(p.pt1) && p1.equals(p.pt2))){
					ArrayList<Point> pts = p.pts;
					Point pt;
					int index;
					for(int j=0;j<pts.size();j++){
						pt = pts.get(j);
						index = pt.x + pt.y*width;
						mask.set(index + activeSlice*length, false);
					}
					remove(p);
					return;
				}
			}
		}
		
		/**
		 * Finds the path (and thus the endpoints) that
		 * contains the given point. Basically searches 
		 * through each LinePath list to find this point.
		 * @param pt
		 * @return
		 */
		private LinePath findPath(Point pt){
			for(int i=0;i<this.size();i++){
				LinePath path = this.get(i);
				if(path.contains(pt)){
					return path;
				}
			}
			return null;
		}
		
	}


	/**
	 * Subclass, mostly used as a container. Keeps track of 
	 * the endpoints and which points are assigned to the
	 * endpoints (as determined by Bresenham's Line Algorithm.
	 * @author wangvg
	 *
	 */
	private class LinePath{
		
		private Point pt1;
		
		private Point pt2;
		
		private ArrayList<Point> pts;
		
		private LinePath(Point p1, Point p2, ArrayList<Point> list){
			pt1 = p1;
			pt2 = p2;
			pts = list;
		}
		
		private boolean contains(Point pt){
			return pts.contains(pt);
		}
		
	}


	/**
	 * Subclass which is basically just an implementation of a multi-
	 * linked list. Keeps track of an individual node and which other 
	 * nodes it is connected to. 
	 * @author wangvg
	 *
	 */
	private class LinkElement{
		
		private Point pt;
		
		private ArrayList<LinkElement> linked;
		
		private LinkElement(Point node){
			pt = node;
			linked = new ArrayList<LinkElement>();
			links.add(this);
		}
		
		private void addLinkTo(LinkElement to){
			linked.add(to);
			to.linked.add(this);
		}
		
		private ArrayList<LinkElement> copyList(){
			ArrayList<LinkElement> list = new ArrayList<LinkElement>();
			list.addAll(linked);
			return list;
		}
		
		private void removeLinkTo(LinkElement to){
			linked.remove(to);
			to.linked.remove(this);
		}
		
	}
	

	/**
	 * A subclass used to keep track of which nodes are connected. Used when
	 * adding/deleting nodes in order to maintain continuous connections between
	 * nodes. Also used when saving the new SWC file in order to properly link
	 * lines in the file.
	 * @author wangvg
	 *
	 */
	private class Linking extends ArrayList<LinkElement>{
		
		/**
		 * 
		 */
		private static final long serialVersionUID = -7735099954298324932L;

		private Linking(){
			super();
		}
		
		/**
		 * Gets an element from the list based on
		 * its point instead of reference
		 * @param p
		 * @return
		 */
		private LinkElement get(Point p){
			
			for(int i=0;i<size();i++){
				LinkElement e = get(i);
				if(p.equals(e.pt)){
					return e;
				}
			}

			return null;
		}
		
		/**
		 * Adds a node at the given point which lies on
		 * the given path. Breaks the previous line into
		 * two parts and adds the VOI to control the new
		 * lines.
		 * @param node
		 * @param path
		 */
		private void addNode(Point node, LinePath path){
			Point to = path.pt1;
			Point from = path.pt2;
			LinkElement eTo = get(to);
			LinkElement eFrom = get(from);
			LinkElement newLink = new LinkElement(node);
			
			eTo.removeLinkTo(eFrom);
			eTo.addLinkTo(newLink);
			eFrom.addLinkTo(newLink);
			
			paths.remove(to, from);
			paths.add(to, node);
			paths.add(node, from);
			
			VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(node.x,node.y,activeSlice));
			voi.setLabel("");
			controlPts.importCurve(voi);
			
			subVolume.notifyImageDisplayListeners();
		}
		
		/**
		 * The move node is implemented as simply breaking
		 * the old connection, moving the node, and reconnecting
		 * the nodes at the new location.
		 * @param from
		 * @param to
		 */
		private void moveNode(Point from, Point to){
			LinkElement node = get(from);
			node.pt = to;
			ArrayList<LinkElement> list = node.linked;
			for(int i=0;i<list.size();i++){
				paths.remove(from, list.get(i).pt);
			}
			for(int i=0;i<list.size();i++){
				paths.add(to, list.get(i).pt);
			}
			
		}
	
		/**
		 * Removes the node in question. It then uses the links
		 * provided to attempt to reconnect the nodes. This is not
		 * optimized and may result in strange behavior possibly
		 * occuring if deleting a node with 3+ connections
		 * @param node
		 */
		private void removeNode(Point node){
			LinkElement e1, e2;
			LinkElement eNode = get(node);
			ArrayList<LinkElement> pts = eNode.linked;
			int size = pts.size();
			e1 = pts.get(0);
			e1.removeLinkTo(eNode);
			paths.remove(node, e1.pt);
			
			for(int i=1; i<size;i++){
				e2 = pts.get(0);
				e2.removeLinkTo(eNode);
				paths.remove(node, e2.pt);
				e2.addLinkTo(e1);
				paths.add(e1.pt, e2.pt);
				e1 = e2;
				
			}	
			remove(eNode);	
		}
		
	}
	
	/**
	 * Subclass used mostly just to keep track of higher order
	 * branches in the neuron. The longest branch is considered
	 * the first order branch, with branches coming off this 
	 * branch considered second order, etc.
	 * 
	 * The comparator is flipped from natural ordering so that 
	 * elements in a Priority Queue are in descending order
	 * @author wangvg
	 *
	 */
	private class NeuronLength implements Comparable<NeuronLength>{
		
		private Point endPt;
		
		private double length;
		
		private int order;
		
		private NeuronLength(Point pt, double l){
			endPt = pt;
			length = l;
			order = 1;
		}

		@Override
		/**
		 * Flipped ordering so that highest priority is highest length
		 * 
		 * Right now, progenitor/origin has highest priority in finding length
		 * This has since been changed to what is seen below.
		 * 
		 * Changed so labeled primary branch has highest priority, with the
		 * progenitor haveing second highest priority
		 */
		public int compareTo(NeuronLength o) {
			
			//Make primary branch highest priority
			if(primaryPt != null){
				if(endPt.equals(primaryPt))
					return -1;
				else if(o.endPt.equals(primaryPt))
					return 1;
			}
			//Progenitor branch has second highest priority
			//Origin branch has the same prioirty, however with the
			//split point, only one will occur in any given search
			if(progenitorPt != null){
				if(endPt.equals(progenitorPt)
						|| endPt.equals(origin))
					return -1;
				else if(o.endPt.equals(progenitorPt)
						|| o.endPt.equals(origin))
					return 1;
			}
			//Longest branch (in the absense of the above) has
			//the third highest priority
			double l0 = this.length;
			double l1 = o.length;
			if(l0 > l1)
				return -1;
			else if(l0 < l1)
				return 1;
			else
				return 0;
		}

		
	}
	
	
}
