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
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
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
	
	private ArrayList<File> images;
	
	private ArrayList<File> swcList;
	
	private int[] imBuffer;
	
	private ModelImage subVolume;
	
	private ViewJFrameImage subVolumeFrame;
	
	private JFrameHistogram histoFrame;
	
	private JTextField dirText;
	
	private JFileChooser fileChooser;
	
	private int currentSlice;
	
	private int prevSlice;
	
	private int sliceRange;
	
	private int numImages;
	
	private int width;
	
	private int height;
	
	private int length;
	
	private int[] extents;
	
	private int type;
	
	private JLabel sliceLabel;
	
	private JLabel centerLabel;
	
	private ModelLUT lut;
	
	private LineList paths;
	
	private Linking links;
	
	private BitSet mask;
	
	private VOI controlPts;
	
	private int depth;
	
	private boolean dragged;
	
	private boolean ptClicked;
	
	private Point toChange;
	
	private JRadioButton addRB;
	
	private JRadioButton deleteRB;
	
	private VOIBase lastActive;
	
	private Point origin;
	
	public PlugInDialogEditNeuron(){
		super();
		
		images = new ArrayList<File>();
		swcList = new ArrayList<File>();
		
		sliceRange = 5;
		currentSlice = 0;
		prevSlice = 0;
		
		init();
		
	}
	
	public PlugInDialogEditNeuron(String directory){
		super();
		
		images = new ArrayList<File>();
		swcList = new ArrayList<File>();
		
		populateImages(new File(directory));
		initEditor();
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
        		dispose();
        		subVolumeFrame.dispose();
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
	}
	
	private ArrayList<Point> bresenham(Point p0, Point p1){
		return bresenham(p0.x, p0.y, p1.x, p1.y);
	}
	
	private ArrayList<Point> bresenham(int x0, int y0, int x1, int y1){
		
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
			for(int k=0;k<depth;k++){
				mask.set(i+k*length);
			}
			
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
		
		Point pt0 = new Point(x0, y0);
		Point pt1 = new Point(x1, y1);
		
		//LinePath path = new LinePath(pt0, pt1, pts);
		//paths.add(path);
		
		return pts;
	}
	
	private void chooseDir(){
		String dirText = Preferences.getImageDirectory();
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addActionListener(this);
		fileChooser.showOpenDialog(this);
	}
	
	private void closeImage(){
		histoFrame.disposeLocal();
		subVolumeFrame.dispose();
	}
	
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
        
        getContentPane().add(choosePanel, BorderLayout.CENTER);

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
	
	private void initEditor(){
		
		System.out.println("start");
		
		getContentPane().removeAll();
		
		FileIO imReader = new FileIO();
		ModelImage sliceIm = imReader.readImage(images.get(0).getAbsolutePath());
		extents = sliceIm.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		type = sliceIm.getType();
		sliceIm.disposeLocal();
		
		JPanel radioPanel = new JPanel();
        radioPanel.setForeground(Color.black);
        
        addRB = new JRadioButton("Add");
        addRB.setFont(serif12);
        addRB.setSelected(true);
        
        deleteRB = new JRadioButton("Delete");
        deleteRB.setFont(serif12);
        
        ButtonGroup group = new ButtonGroup();
        
        group.add(addRB);
        group.add(deleteRB);
        radioPanel.add(addRB);
        radioPanel.add(deleteRB);
        getContentPane().add(radioPanel, BorderLayout.NORTH);
        
		JPanel labelPanel = new JPanel();
		labelPanel.setForeground(Color.black);
		
		sliceLabel = new JLabel("Centered Slice: 0");
		sliceLabel.setFont(serif12);
		labelPanel.add(sliceLabel);
		
		
		JPanel centerPanel = new JPanel();
		centerPanel.setForeground(Color.black);
		
		centerLabel = new JLabel("Center Depth: 0");
		centerLabel.setFont(serif12);
		centerPanel.add(centerLabel);
		
		PanelManager manage = new PanelManager();
		manage.add(labelPanel);
		manage.addOnNextLine(centerPanel);
		getContentPane().add(manage.getPanel(), BorderLayout.CENTER);
		
		JPanel boxPanel = new JPanel(/*new GridLayout(1,2)*/);
        boxPanel.setForeground(Color.black);
        
        JButton prevButton = new JButton("Prev");
        prevButton.setFont(serif12);
        prevButton.addActionListener(this);
        boxPanel.add(prevButton);
        
        JButton nextButton = new JButton("Next");
        nextButton.setFont(serif12);
        nextButton.addActionListener(this);
        boxPanel.add(nextButton);
        
        getContentPane().add(boxPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        System.gc();
		
	}
	
	private void openHisto(){
		ViewJComponentEditImage comp = subVolumeFrame.getComponentImage();
		histoFrame = new JFrameHistogram(parentFrame, subVolume, null, comp.getLUTa(), null);
		histoFrame.histogramLUT(true, true);
	}
	
	private void openImage(){
		
		int lowerBound = Math.max(0, currentSlice - sliceRange);
		int upperBound = Math.min(numImages - 1, currentSlice + sliceRange);
		depth = upperBound - lowerBound + 1;
		//int sliceDiff = currentSlice - prevSlice;
		int[] sliceBuffer = new int[length];
		
		//Reset certain things here;
		mask = new BitSet(length*depth);
		controlPts = new VOI((short) 0, "Control Points", VOI.POINT, 0);
		paths = new LineList();
		links = new Linking();
		
		
		FileIO imReader = new FileIO();
		ModelImage sliceIm = null;
		
		try{
			if(currentSlice == prevSlice){//Initial case
				imBuffer = new int[length*depth];
				for(int i=0;i<depth;i++){
					sliceIm = imReader.readImage(images.get(i+lowerBound).getAbsolutePath());
					sliceIm.exportData(0, length, sliceBuffer);
					System.arraycopy(sliceBuffer, 0, imBuffer, i*length, length);
					sliceIm.disposeLocal();
				}
			} else if(currentSlice < prevSlice){//Going backwards
				int[] tempBuffer = new int[length*depth];
				int prevLBound = Math.max(0, prevSlice - sliceRange);
				//int prevUBound = Math.min(numImages - 1, prevSlice + sliceRange);
				int copyDepth = upperBound - prevLBound + 1 ;
				int lBoundDiff = prevLBound - lowerBound;
				//int uBoundDiff = prevUBound - upperBound;
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

			String label = sliceLabel.getText();
			String[] parts = label.split(" ");
			String sliceStr = String.valueOf(currentSlice);
			parts[2] = String.valueOf(currentSlice - lowerBound);
			sliceLabel.setText(parts[0] + " " + parts[1] + " " + parts[2]);
			
			label = centerLabel.getText();
			parts = label.split(" ");
			
			centerLabel.setText(parts[0] + " " + parts[1] + " " + sliceStr);
			
			subVolume = new ModelImage(type, new int[]{width, height, depth}, "Sub-Volume");
			subVolume.importData(0, imBuffer, true);
			subVolumeFrame = new ViewJFrameImage(subVolume, lut);
			subVolumeFrame.setSlice(currentSlice - lowerBound);
		
		} catch(IOException e){
			e.printStackTrace();
		}

		readSWC();
		
		subVolumeFrame.getComponentImage().setPaintMask(mask);
		subVolumeFrame.getComponentImage().addMouseListener(this);
		subVolumeFrame.getComponentImage().addMouseMotionListener(this);
		subVolumeFrame.getControls().getTools().setOpacity(1.0f);
		subVolumeFrame.getControls().getTools().setPaintColor(Color.WHITE);
		subVolumeFrame.updateImages();
		
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
	
	private void readSWC(){

		ArrayList<String[]> points = null;
		String[] lineArray;
		try {
			BufferedReader input =  new BufferedReader(new FileReader(swcList.get(currentSlice)));
			String line = null; 
			points = new ArrayList<String[]>();
			while (( line = input.readLine()) != null){
				if(line.startsWith("#"))
					continue;
				lineArray = line.split(" ");
				points.add(lineArray);
			}
			input.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;
		} catch (IOException io){
			io.printStackTrace();
			return;
		}

		int linkedTo;
		int x0, x1, y0, y1;
		Point p0, p1;
		String num;
		LinkElement e0, e1;
		
		//for(int i=1;i<points.size();i++){
		for(int i=points.size()-1;i>0;i--){
			lineArray = points.get(i);
			num = lineArray[2];
			num = num.substring(0, num.indexOf("."));
			x0 = Integer.parseInt(num);
			num = lineArray[3];
			num = num.substring(0, num.indexOf("."));
			y0 = height - Integer.parseInt(num);
			
			p0 = new Point(x0, y0);
			
			//Make the various line VOIs
			linkedTo = Integer.parseInt(lineArray[6]);
			if(linkedTo == -1){
				//This is the end
				//Should never reach this though, as we 
				//never check the first element
			}
			lineArray = points.get(linkedTo - 1);
			num = lineArray[2];
			num = num.substring(0, num.indexOf("."));
			x1 = Integer.parseInt(num);
			num = lineArray[3];
			num = num.substring(0, num.indexOf("."));
			y1 = height - Integer.parseInt(num);
			
			p1 = new Point(x1, y1);
			
			e0 = links.get(p0);
			if(e0 == null){
				e0 = new LinkElement(p0, p1);
				links.add(e0);
			} else{
				e0.addLinkTo(p1);
			}
			
			e1 = links.get(p1);
			if(e1 == null){
				e1 = new LinkElement(p1, p0);
				links.add(e1);
			} else{
				e1.addLinkTo(p0);
			}
			
			paths.add(p0, p1);
			
			for(int k=0;k<depth;k++){
				VOIPoint voi = new VOIPoint(3, new Vector3f(x0,y0,k));
				voi.setLabel("");
				controlPts.importCurve(voi);
				//controlPts.importPoint(new Vector3f(x0,y0,k));
			}
			
			//System.out.println(x0 + ", " + y0 + " " + x1 + ", " + y1);
		}
		
		lineArray = points.get(0);
		num = lineArray[2];
		num = num.substring(0, num.indexOf("."));
		x0 = Integer.parseInt(num);
		num = lineArray[3];
		num = num.substring(0, num.indexOf("."));
		y0 = height - Integer.parseInt(num);
		
		origin = new Point(x0,y0);
		for(int k=0;k<depth;k++){
			VOIPoint voi = new VOIPoint(3, new Vector3f(x0,y0,k));
			voi.setLabel("");
			controlPts.importCurve(voi);
			//controlPts.importPoint(new Vector3f(x0,y0,k));
		}
		
		
		subVolume.getVOIs().add(controlPts);
		subVolume.notifyImageDisplayListeners(null, true, 0, -1);
		
		/*LinePath path;
		ArrayList<Point> list;
		Point pt;
		int index;
		
		for(int i=0;i<paths.size();i++){
			path = paths.get(i);
			list = path.pts;
			for(int j=0;j<list.size();j++){
				pt = list.get(j);
				index = pt.x + pt.y * width;
				for(int k=0;k<depth;k++){
					mask.set(index + k*length);
				}
			}
		}*/
		
	}
	
	private void saveNewSWC(){
		
	}
	
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

		if(ptClicked)
			dragged = true;
			
	}

	@Override
	public void mouseMoved(MouseEvent e) {

		
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		
		int numClicks = e.getClickCount();
		int button = e.getButton();
		
		if(numClicks < 2 || button != MouseEvent.BUTTON1)
			return;
		
		float zoomX = subVolumeFrame.getComponentImage().getZoomX();
		float zoomY = subVolumeFrame.getComponentImage().getZoomY();
		int x = (int) ((float)e.getX()/zoomX); //- left;
		int y = (int) ((float)e.getY()/zoomY); //- top;
		
		
		if(addRB.isSelected()){
			System.out.println("Double left click on: (" + x + ", " + y + ")");
			
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
			System.out.println("Searching for: (" + x + ", " + y + ")");
			LinePath onPath = paths.findPath(new Point(x, y));
			if(onPath == null)
				System.out.println("Did not find path");
			else{
				links.addNode(new Point(x, y), onPath.pt1, onPath.pt2);
			}
			
		} else if (deleteRB.isSelected()){
			//search list of VOIs for this point
			int slice = subVolumeFrame.getComponentImage().getSlice();
			VOIBaseVector pts = controlPts.getCurves();
			VOIBase activeVOI = lastActive;

			if(activeVOI != null){
				Vector<VOIBase> vec = controlPts.getSliceCurves(slice);
				int index = vec.indexOf(activeVOI);
				for(int k=0;k<depth;k++){
					vec = controlPts.getSliceCurves(k);
					VOIBase basevoi = vec.get(index);
					pts.remove(basevoi);
				}
			}
			
			VOIPoint ptVOI = (VOIPoint) activeVOI;
			Vector3f ptVec = ptVOI.exportPoint();
			Point coord = new Point((int)ptVec.X, (int)ptVec.Y);
			if(coord.equals(origin)){
				LinkElement elem = links.get(origin);
				origin = elem.linkedTo.get(0);
			}
				
			links.removeNode(coord);
			
			
			
			lastActive = null;
			
			subVolume.notifyImageDisplayListeners();
			//Vector<Vector3f> slicePts = controlPts.exportPoints(0);
			//Vector3f thisPt = new Vector3f(x, y, 0);
			
				//Remove VOI from list, as well as other things

			
		}
		
		controlPts.setAllActive(false);
		ptClicked = false;

		
	}

	@Override
	public void mousePressed(MouseEvent e) {
		
		//float zoomX = subVolumeFrame.getComponentImage().getZoomX();
		//float zoomY = subVolumeFrame.getComponentImage().getZoomY();
		//int x = (int) ((float)e.getX()/zoomX); //- left;
		//int y = (int) ((float)e.getY()/zoomY); //- top;

		//int ind = x + y*width;

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
			//LinePath path = paths.findPath(pt);
			System.out.println("VOI selected");
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {

		
		//update position of the control points in the line structures if the mouse was dragged
		//which implies the VOI was moved (also need to check if the VOI was selected);
		if(dragged){
			dragged = false;
			ptClicked = false;
			Vector3f ptVec = ((VOIPoint)lastActive).exportPoint();
			int x = (int) ptVec.X;
			int y = (int) ptVec.Y;
			
			if(toChange.equals(origin))
				origin = new Point(x,y);
			
			System.out.println("VOI was dragged");
			//update positions
			/*float zoomX = subVolumeFrame.getComponentImage().getZoomX();
			float zoomY = subVolumeFrame.getComponentImage().getZoomY();
			int x = (int) ((float)e.getX()/zoomX); //- left;
			int y = (int) ((float)e.getY()/zoomY); //- top;*/
			
			//We know that the drag happened on a VOI, so now update all links and lines
			
			int ox = toChange.x;
			int oy = toChange.y;
			
			System.out.printf("From (%d, %d) to (%d, %d)\n", ox, oy, x, y);
			
			links.moveNode(toChange, new Point(x,y));
			
			lastActive = null;
			toChange = null;
		}
		
		controlPts.setAllActive(false);
	}

	@Override
	public void mouseEntered(MouseEvent e) {

	}

	@Override
	public void mouseExited(MouseEvent e) {
		
	}
	
	private class Linking extends ArrayList<LinkElement>{
		
		private Linking(){
			super();
		}
		
		private LinkElement get(Point p){
			
			for(int i=0;i<size();i++){
				LinkElement e = get(i);
				if(p.equals(e.pt)){
					return e;
				}
			}
			/*Iterator<LinkElement> iter = this.iterator();
			while(iter.hasNext()){
				LinkElement e = iter.next();
				if(p == e.pt)
					return e;
			}*/
			return null;
		}
		
		private void addNode(Point node, Point to, Point from){
			LinkElement eTo = get(to);
			LinkElement eFrom = get(from);
			
			eTo.removeLinkTo(from);
			eTo.addLinkTo(node);
			eFrom.removeLinkTo(to);
			eFrom.addLinkTo(node);
			
			LinkElement newLink = new LinkElement(node,to);
			newLink.addLinkTo(from);
			add(newLink);
			paths.remove(to, from);
			paths.add(to, node);
			paths.add(node, from);
			
			for(int k=0;k<depth;k++){
				Vector3f vecPt = new Vector3f(node.x, node.y, k);
				controlPts.importPoint(vecPt);
			}
			
			subVolume.notifyImageDisplayListeners();
			
			System.out.println("Node added on: (" + node.x + ", " + node.y + ")");

		}
		
		private void moveNode(Point from, Point to){
			LinkElement node = get(from);
			node.pt = to;
			ArrayList<Point> list = node.linkedTo;
			for(int i=0;i<list.size();i++){
				LinkElement linked = get(list.get(i));
				linked.linkedTo.remove(from);
				linked.linkedTo.add(to);
				paths.remove(from, linked.pt);
				//paths.add(to, linked.pt);
				
			}
			for(int i=0;i<list.size();i++){
				LinkElement linked = get(list.get(i));
				paths.add(to, linked.pt);
			}
			
			int slice = subVolumeFrame.getComponentImage().getSlice();
			
			Vector<VOIBase> vec = controlPts.getSliceCurves(slice);
			int index = vec.indexOf(lastActive);
			int dx = to.x - from.x;
			int dy = to.y - from.y;
			for(int k=0;k<depth;k++){
				if(k == slice) continue;
				vec = controlPts.getSliceCurves(k);
				VOIPoint basevoi = (VOIPoint)vec.get(index);
				basevoi.moveVOIPoint(dx, dy, 0, width, height, depth);
			}
			
			subVolume.notifyImageDisplayListeners();
			subVolumeFrame.updateImages();
		}
		
		private void removeNode(Point node){
			LinkElement e1, e2;
			LinkElement eNode = get(node);
			ArrayList<Point> pts = eNode.linkedTo;
			e1 = get(pts.get(0));

			e1.removeLinkTo(node);
			paths.remove(node, e1.pt);
			for(int i=1; i<pts.size();i++){
				e2 = get(pts.get(i));
				e2.removeLinkTo(node);
				paths.remove(node, e2.pt);
				e2.addLinkTo(e1.pt);
				e1.addLinkTo(e2.pt);
				paths.add(e1.pt, e2.pt);
				e1 = e2;
				
			}
			
			remove(eNode);
		}
		
	}
	
	private class LinkElement{
		
		private Point pt;
		
		private ArrayList<Point> linkedTo;
		
		private LinkElement(Point node, Point to){
			pt = node;
			linkedTo = new ArrayList<Point>();
			linkedTo.add(to);
		}
		
		private void addLinkTo(Point to){
			linkedTo.add(to);
		}
		
		private void removeLinkTo(Point to){
			linkedTo.remove(to);
		}
		
	}
	
	private class LineList extends ArrayList<LinePath>{
		
		private LineList(){
			super();
		}
		
		private void remove(Point p1, Point p2){
			Iterator<LinePath> iter = this.iterator();
			while(iter.hasNext()){
				LinePath p = iter.next();
				if((p1.equals(p.pt1) && p2.equals(p.pt2)) || 
						(p2.equals(p.pt1) && p1.equals(p.pt2))){
					ArrayList<Point> pts = p.pts;
					Point pt;
					int index;
					for(int i=0;i<pts.size();i++){
						pt = pts.get(i);
						index = pt.x + pt.y*width;
						for(int k=0;k<depth;k++){
							mask.set(index + k*length, false);
						}
					}
					remove(p);
					return;
				}
			}
		}
		
		private void add(Point p1, Point p2){
			ArrayList<Point> list = bresenham(p1, p2);
			this.add(new LinePath(p1, p2, list));
		}
		
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
	
}
