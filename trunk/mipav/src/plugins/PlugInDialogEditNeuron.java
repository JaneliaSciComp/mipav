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
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashSet;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
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
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.JFrameHistogram;
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
	
	private int activeSlice;
	
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
		
		sliceRange = 5;
		currentSlice = 0;
		prevSlice = 0;
		
		populateImages(new File(directory));
		initEditor();
		openImage();
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
        		if (isExitRequired()) {
    	            System.exit(0);
    	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
    	        } else {
    	        	dispose();
    	        	subVolumeFrame.dispose();
    	        }
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
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	subVolumeFrame.close();
	        	dispose();
	        }
		}
	}
	
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
	
	private ArrayList<Point> bresenham2(Point p0, Point p1){
		
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
		
		//Point pt0 = new Point(x0, y0);
		//Point pt1 = new Point(x1, y1);
		
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
		
		
		JPanel boxPanel = new JPanel(new GridLayout(1,2));
        boxPanel.setForeground(Color.black);
        
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
	
	private void readSWC(int lowerBound) throws FileNotFoundException, IOException{
		
		mask = new BitSet(length*depth);
		controlPts = new VOI((short) 0, "Control Points", VOI.POINT, 0);
		paths = new LineList();
		links = new Linking();
		
		ArrayList<String[]> points = null;
		String[] lineArray;
		
		for(int k=0;k<depth;k++){
			BufferedReader input =  new BufferedReader(new FileReader(swcList.get(k + lowerBound)));
			String line = null; 
			points = new ArrayList<String[]>();
			while (( line = input.readLine()) != null){
				if(line.startsWith("#"))
					continue;
				lineArray = line.split(" ");
				points.add(lineArray);
			}
			input.close();
			
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
					
					
					paths.add2(p0, p1);
					
					VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0,y0,activeSlice));
					voi.setLabel("");
					controlPts.importCurve(voi);
					//controlPts.importPoint(new Vector3f(x0,y0,k));
				}
			}
			
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
				voi.setLabel("");
				controlPts.importCurve(voi);
			}
				//controlPts.importPoint(new Vector3f(x0,y0,k));
		}
		
		subVolume.getVOIs().add(controlPts);
		subVolume.notifyImageDisplayListeners(null, true, 0, -1);
		
		subVolumeFrame.getComponentImage().setPaintMask(mask);
		subVolumeFrame.getComponentImage().addMouseListener(this);
		subVolumeFrame.getComponentImage().addMouseMotionListener(this);
		subVolumeFrame.getControls().getTools().setOpacity(1.0f);
		subVolumeFrame.getControls().getTools().setPaintColor(Color.WHITE);
		subVolumeFrame.updateImages();
		
	}
	
	private void saveNewSWC() throws IOException{
		
		File currentSWC = swcList.get(currentSlice);
		String saveName = currentSWC.getPath();
		String name = currentSWC.getName();
		String parent = currentSWC.getParent();
		int splitAt = name.lastIndexOf(".");
		String fileName = name.substring(0, splitAt) + "_old";
		String fileExt = name.substring(splitAt + 1, name.length());
		String newName = fileName + "." + fileExt;
		File oldSWC = new File(parent + File.separator + newName);
		if(oldSWC.exists()){
			oldSWC.delete();
		}
		currentSWC.renameTo(oldSWC);
		currentSWC = new File(saveName);
		
		FileWriter writer = new FileWriter(currentSWC);
		
		try {
			BufferedReader input =  new BufferedReader(new FileReader(oldSWC));
			String line = null; 
			while (( line = input.readLine()) != null){
				if(line.startsWith("#"))
					writer.append(line + "\n");
			}
			input.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return;
		}
		
		int line = 1;
		LinkElement start = links.get(origin);
		Point pt = start.pt;
		int[] parts = new int[7];
		//String[] parts = new String[7];
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
			
			System.out.println(" ");
		}
		
		writer.close();
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
				links.addNode2(new Point(x, y), onPath);
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
			}
				
			links.removeNode2(coord);

			lastActive = null;
			
			subVolume.notifyImageDisplayListeners();

		}
		
		controlPts.setAllActive(false);
		ptClicked = false;

		
	}

	@Override
	public void mousePressed(MouseEvent e) {

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

	@Override
	public void mouseReleased(MouseEvent e) {

		ptClicked = false;
		//update position of the control points in the line structures if the mouse was dragged
		//which implies the VOI was moved (also need to check if the VOI was selected);
		if(dragged){
			dragged = false;
			
			Vector3f ptVec = ((VOIPoint)lastActive).exportPoint();
			int x = (int) ptVec.X;
			int y = (int) ptVec.Y;
			
			if(toChange.equals(origin))
				origin = new Point(x,y);
			
			//We know that the drag happened on a VOI, so now update all links and lines
			
			links.moveNode2(toChange, new Point(x,y));
			
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

	private class Linking extends ArrayList<LinkElement>{
		
		/**
		 * 
		 */
		private static final long serialVersionUID = -7735099954298324932L;

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

			return null;
		}
		
		/*private void addNode(Point node, LinePath path){
			Point to = path.pt1;
			Point from = path.pt2;
			LinkElement eTo = get(to);
			LinkElement eFrom = get(from);
			LinkElement newLink = new LinkElement(node);
			
			eTo.removeLinkTo(eFrom);
			eTo.addLinkTo(newLink);
			eFrom.addLinkTo(newLink);
			//add(newLink);
			
			paths.remove(to, from);
			paths.add(to, node);
			paths.add(node, from);
			
			for(int k=0;k<depth;k++){
				VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(node.x,node.y,k));
				voi.setLabel("");
				controlPts.importCurve(voi);
			}
			
			subVolume.notifyImageDisplayListeners();
		}*/
		
		/*private void moveNode(Point from, Point to){
			LinkElement node = get(from);
			node.pt = to;
			ArrayList<LinkElement> list = node.linked;
			for(int i=0;i<list.size();i++){
				paths.remove(from, list.get(i).pt);
			}
			for(int i=0;i<list.size();i++){
				paths.add(to, list.get(i).pt);
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
			
		}*/
	
		/*private void removeNode(Point node){
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
				//e1.addLinkTo(e2);
				paths.add(e1.pt, e2.pt);
				e1 = e2;
				
			}
			
			remove(eNode);
			
		}*/
		
		private void addNode2(Point node, LinePath path){
			Point to = path.pt1;
			Point from = path.pt2;
			LinkElement eTo = get(to);
			LinkElement eFrom = get(from);
			LinkElement newLink = new LinkElement(node);
			
			eTo.removeLinkTo(eFrom);
			eTo.addLinkTo(newLink);
			eFrom.addLinkTo(newLink);
			//add(newLink);
			
			paths.remove2(to, from);
			paths.add2(to, node);
			paths.add2(node, from);
			
			VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(node.x,node.y,activeSlice));
			voi.setLabel("");
			controlPts.importCurve(voi);
			
			subVolume.notifyImageDisplayListeners();
		}
		
		private void moveNode2(Point from, Point to){
			LinkElement node = get(from);
			node.pt = to;
			ArrayList<LinkElement> list = node.linked;
			for(int i=0;i<list.size();i++){
				paths.remove2(from, list.get(i).pt);
			}
			for(int i=0;i<list.size();i++){
				paths.add2(to, list.get(i).pt);
			}
			
		}
	
		private void removeNode2(Point node){
			LinkElement e1, e2;
			LinkElement eNode = get(node);
			ArrayList<LinkElement> pts = eNode.linked;
			int size = pts.size();
			e1 = pts.get(0);
			e1.removeLinkTo(eNode);
			paths.remove2(node, e1.pt);
			
			for(int i=1; i<size;i++){
				e2 = pts.get(0);
				e2.removeLinkTo(eNode);
				paths.remove2(node, e2.pt);
				e2.addLinkTo(e1);
				//e1.addLinkTo(e2);
				paths.add2(e1.pt, e2.pt);
				e1 = e2;
				
			}
			
			remove(eNode);
			
		}
		
	}
	
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
		
		private void removeLinkTo(LinkElement to){
			linked.remove(to);
			to.linked.remove(this);
		}
		
	}
	
	private class LineList extends ArrayList<LinePath>{
		
		/**
		 * 
		 */
		private static final long serialVersionUID = 6977450762745726017L;

		private LineList(){
			super();
		}
		
		/*private void add(Point p1, Point p2){
			ArrayList<Point> list = bresenham(p1, p2);
			this.add(new LinePath(p1, p2, list));
		}*/
		
		/*private void remove(Point p1, Point p2){
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
		}*/
		
		private void add2(Point p1, Point p2){
			ArrayList<Point> list = bresenham2(p1,p2);
			this.add(new LinePath(p1, p2, list));
		}
		
		private void remove2(Point p1, Point p2){
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
	
	/*private void readSWC(){

	mask = new BitSet(length*depth);
	controlPts = new VOI((short) 0, "Control Points", VOI.POINT, 0);
	paths = new LineList();
	links = new Linking();
	
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
		e1 = links.get(p1);
		
		if(e0 == null){
			e0 = new LinkElement(p0);
		}
		if(e1 == null){
			e1 = new LinkElement(p1);
		}
		e0.addLinkTo(e1);
		
		
		paths.add(p0, p1);
		
		for(int k=0;k<depth;k++){
			VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0,y0,k));
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
		VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0,y0,k));
		voi.setLabel("");
		controlPts.importCurve(voi);
		//controlPts.importPoint(new Vector3f(x0,y0,k));
	}
	
	
	subVolume.getVOIs().add(controlPts);
	subVolume.notifyImageDisplayListeners(null, true, 0, -1);
	
	subVolumeFrame.getComponentImage().setPaintMask(mask);
	subVolumeFrame.getComponentImage().addMouseListener(this);
	subVolumeFrame.getComponentImage().addMouseMotionListener(this);
	subVolumeFrame.getControls().getTools().setOpacity(1.0f);
	subVolumeFrame.getControls().getTools().setPaintColor(Color.WHITE);
	subVolumeFrame.updateImages();
	
}*/
	
	
	/*private ArrayList<Point> bresenham(Point p0, Point p1){
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
		
		//Point pt0 = new Point(x0, y0);
		//Point pt1 = new Point(x1, y1);
		
		//LinePath path = new LinePath(pt0, pt1, pts);
		//paths.add(path);
		
		return pts;
	}*/
	
}
