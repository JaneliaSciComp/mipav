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

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
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
	
	//private ViewJFrameImage sliceFrame;
	
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
	
	private ModelLUT lut;
	
	
	
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
	
	private void bresenham(int x0, int y0, int x1, int y1){
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
		
		while(true){
			//plot(x0,y0);
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
		
		getContentPane().removeAll();
		
		FileIO imReader = new FileIO();
		ModelImage sliceIm = imReader.readImage(images.get(0).getAbsolutePath());
		extents = sliceIm.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		type = sliceIm.getType();
		sliceIm.disposeLocal();
		
		JPanel labelPanel = new JPanel();
		labelPanel.setForeground(Color.black);
		
		sliceLabel = new JLabel("Center Slice: 0");
		sliceLabel.setFont(serif12);
		labelPanel.add(sliceLabel);
		getContentPane().add(labelPanel, BorderLayout.CENTER);
		
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
		int depth = upperBound - lowerBound + 1;
		//int sliceDiff = currentSlice - prevSlice;
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
					sliceIm.disposeLocal();
				}
			} else if(currentSlice < prevSlice){//Going backwards
				int[] tempBuffer = new int[length*depth];
				int prevLBound = Math.max(0, prevSlice - sliceRange);
				int prevUBound = Math.min(numImages - 1, prevSlice + sliceRange);
				int copyDepth = upperBound - prevLBound + 1 ;
				int lBoundDiff = prevLBound - lowerBound;
				int uBoundDiff = prevUBound - upperBound;
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
			parts[2] = String.valueOf(currentSlice);
			sliceLabel.setText(parts[0] + " " + parts[1] + " " + parts[2]);
			
			subVolume = new ModelImage(type, new int[]{width, height, depth}, "Sub-Volume");
			subVolume.importData(0, imBuffer, true);
			subVolumeFrame = new ViewJFrameImage(subVolume, lut);
			subVolumeFrame.setSlice(currentSlice - lowerBound);
		
		} catch(IOException e){
			e.printStackTrace();
		}

		//readSWC();
		
		
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

		//for(int i=1;i<points.size();i++){
		for(int i=points.size()-1;i>0;i++){
			lineArray = points.get(i);
			//Make the various line VOIs
		}
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

		
	}

	@Override
	public void mouseMoved(MouseEvent e) {

		
	}

	@Override
	public void mouseClicked(MouseEvent e) {

		
	}

	@Override
	public void mousePressed(MouseEvent e) {

		
	}

	@Override
	public void mouseReleased(MouseEvent e) {

		
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
			
			Iterator<LinkElement> iter = this.iterator();
			while(iter.hasNext()){
				LinkElement e = iter.next();
				if(p == e.pt)
					return e;
			}
			return null;
		}
		
		private void addNode(Point node, Point to, Point from){
			LinkElement eTo = get(to);
			LinkElement eFrom = get(from);
			
		}
		
		private void removeNode(Point node){
			LinkElement e1, e2;
			LinkElement eNode = get(node);
			ArrayList<Point> pts = eNode.linkedTo;
			e1 = get(pts.get(0));

			e1.removeLinkTo(node);
			for(int i=1; i<pts.size();i++){
				e2 = get(pts.get(i));
				e2.removeLinkTo(node);
				e2.addLinkTo(e1.pt);
				e1.addLinkTo(e2.pt);
				
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
	
}
