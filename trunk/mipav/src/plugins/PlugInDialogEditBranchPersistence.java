import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Set;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerListModel;
import javax.swing.SpinnerNumberModel;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.filters.AlgorithmMean;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlices;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.JFrameHistogram;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;


public class PlugInDialogEditBranchPersistence extends JDialogStandalonePlugin implements MouseListener, MouseMotionListener,
ItemListener
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 2186497167180425534L;

	private ArrayList<Integer> spinnerList;
	
	private JSpinner spinner;
	
	/**
	 * Slice in the subvolume that is considered the "center"
	 * which means which slice is editable
	 */
	private int activeSlice;

	private JRadioButton addRB;
	
	/**
	 * VOI containing the points to edit the neuron traces
	 */
	private VOI controlPts;
	
	//private JCheckBox csvBox;

	/**
	 * The active slice's actual depth in the full volume
	 */
	private int currentSlice;
	
	private JRadioButton deleteRB;
	
	//private JCheckBox deleteBox;
	
	/**
	 * Depth of the current subvolume
	 */
	private int depth;
	
	private JTextField dirText;

	private JFileChooser fileChooser;
	
	private int height;
	
	private ArrayList<File> images;
	
	private int[] imBuffer;
	
	private JLabel imName;
	
	/**
	 * Used to keep track of which node to delete
	 */
	private VOIBase lastActive;
	
	private int length;
	
	/**
	 * Store LUT so it is the same between images in the
	 * series
	 */
	private ModelLUT lut;
	
	/**
	 * Neuron traces are overlayed on the subvolume
	 */
	private BitSet mask;
	
	private JCheckBox noiseBox;

	private int numImages;
	
	/**
	 * Keeps track of the original start of the neuron
	 */
	//private Point origin;

	/**
	 * Which slice was the previous active slice. Should only
	 * be a difference of 1, but if an image list is implemented
	 * later, could be differet
	 */
	private int prevSlice;

	/**
	 * Keeps track of which VOI has been force labeled as the
	 * progenitor axon tip
	 */
	private Point progenitorPt;
	
	//private JCheckBox saveBox;
	
	private int sliceRange;

	private JSpinner rangeField;
	
	//private FileWriter statsCSV;

	/**
	 * Part of the 3D image to display on screen
	 */
	private ModelImage subVolume;
	
	/**
	 * The associated frame for the subvolume image
	 */
	private ViewJFrameImage subVolumeFrame;
	
	private ArrayList<File> swcList;
	
	private int width;
	
	private ModelImage imStack = null;
	
	private Point origin;
	
	private VOIPoint originVOI;
	
	private VOIPoint progenitorVOI;
	
	private Hashtable<Integer, VOIPoint> voiHash;
	
	private ArrayList<ArrayList<Integer>> labelList;
	
	private ArrayList<Hashtable<Integer, VOIPoint>> hashList;
	
	private JCheckBox disableBox;
	
	private JCheckBox hideVOIBox;
	
	/**
	 * Primary constructor. Initializes a dialog to ask the user
	 * for a directory to use
	 */
	public PlugInDialogEditBranchPersistence(){
		super();
		
		images = new ArrayList<File>();
		swcList = new ArrayList<File>();
		spinnerList = new ArrayList<Integer>();
		for(int i=1;i<=50;i++){
			spinnerList.add(i);
		}
		
		currentSlice = 0;
		prevSlice = 0;
		
		labelList = new ArrayList<ArrayList<Integer>>();
		hashList = new ArrayList<Hashtable<Integer, VOIPoint>>();
		
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
			File directory = new File(dirText.getText());
			if(!directory.exists()){
				MipavUtil.displayError("File does not exist");
				return;
			}
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
				MipavUtil.displayError("Could not read range value");
				e.printStackTrace();
			}
			if(populateImages(directory)){
				if(initVars()){
					initEditor();
		        	openImage();
				}
			} else 
				MipavUtil.displayError("No compatible SWC files were found");
		}
		else if(command.equals("Next")){
			Set<Integer> keys = voiHash.keySet();
			ArrayList<Integer> keyList = new ArrayList<Integer>(keys);
			Collections.sort(keyList);
			labelList.add(keyList);
			hashList.add(voiHash);
			prevSlice = currentSlice;
			currentSlice++;
			lut = subVolumeFrame.getLUTa();
			float zX = subVolumeFrame.getComponentImage().getZoomX();
			float zY = subVolumeFrame.getComponentImage().getZoomY();
			
        	if(currentSlice < numImages){
        		
        		if(currentSlice > 1){
        			int toAdd = 0;
        			ArrayList<Integer> prev = labelList.get(currentSlice - 1);
        			ArrayList<Integer> prev2 = labelList.get(currentSlice - 2);
        			for(int i=0;i<prev2.size();i++){
        				if(!prev.contains(prev2.get(i))){
        					spinnerList.remove(prev2.get(i));
        					toAdd++;
        				}
        			}
        			int max = spinnerList.get(spinnerList.size()-1);
        			for(int i=max+1;i<=max+toAdd+1;i++){
        				spinnerList.add(i);
        			}
        			SpinnerListModel model = (SpinnerListModel) spinner.getModel();
        			model.setList(spinnerList);
        			
        		}
        		spinner.setValue(spinnerList.get(0));
        		Point loc = subVolumeFrame.getLocation();
        		Dimension dimSize = subVolumeFrame.getSize();
        		subVolumeFrame.close();
        		openImage();
        		subVolumeFrame.setLocation(loc);
        		subVolumeFrame.getComponentImage().setZoom(zX, zY);
    			//subVolumeFrame.updateFrame(zX, zY);
    			subVolumeFrame.setSize(dimSize);
    			subVolumeFrame.updateImages();
        	} else {
        		actionPerformed(new ActionEvent(this, 0, "End"));
        	}
		} else if(command.equals("Prev")){
			if(currentSlice > 0 ){
				float zX = subVolumeFrame.getComponentImage().getZoomX();
				float zY = subVolumeFrame.getComponentImage().getZoomY();
				lut = subVolumeFrame.getLUTa();
				prevSlice = currentSlice;
				currentSlice--;
				if(currentSlice > 0){
					//labelList.remove(currentSlice);
					//hashList.remove(currentSlice);
					ArrayList<Integer> prev = labelList.get(currentSlice - 1);
					int numRem = 0;
					for(int i=0;i<prev.size();i++){
						if(!spinnerList.contains(prev.get(i))){
							numRem++;
							spinnerList.add(prev.get(i));
						}
					}
					Collections.sort(spinnerList);
					for(int i=0;i<numRem;i++){
						spinnerList.remove(spinnerList.size()-1);
					}
				}
				
        		Point loc = subVolumeFrame.getLocation();
        		Dimension dimSize = subVolumeFrame.getSize();
        		subVolumeFrame.close();
        		openImage();
        		
        		ArrayList<Integer> keyList = labelList.remove(currentSlice);
        		voiHash = hashList.remove(currentSlice);
        		
        		if(!voiHash.isEmpty()){
        			for(int i=0;i<keyList.size();i++){
        				int key = keyList.get(i);
        				VOIPoint voi = voiHash.get(key).clone();
        				controlPts.importCurve(voi);
        			}
        			subVolume.getVOIs().add(controlPts);
        			subVolume.notifyImageDisplayListeners(null, true, 0, -1);
        		}

        		subVolumeFrame.setLocation(loc);
        		subVolumeFrame.getComponentImage().setZoom(zX, zY);
    			//subVolumeFrame.updateFrame(zX, zY);
    			subVolumeFrame.setSize(dimSize);
    			subVolumeFrame.updateImages();
			}
		}
		else if(command.equals("Reset")){
			voiHash.clear();
			VOIBaseVector pts = controlPts.getCurves();
			
			for(int i=0;i<pts.size();i++){
				VOIPoint current = (VOIPoint)pts.get(i);
				current.setLabel("");
				Vector3f pt = current.exportPoint();
				Point check = new Point((int)pt.X, (int)pt.Y);
				if(check.equals(origin))
					current.setLabel("O");
				if(check.equals(progenitorPt))
					current.setLabel("P");
			}
			subVolume.notifyImageDisplayListeners(null, true, 0, -1);
			spinner.setValue(spinnerList.get(0));
			
		} else if(command.equals("LUT")){
			openHisto();
		} else if(command.equals("Save")){
			if(currentSlice < numImages){ //Ends before finishing entire stack
				Set<Integer> keys = voiHash.keySet();
				ArrayList<Integer> keyList = new ArrayList<Integer>(keys);
				Collections.sort(keyList);
				labelList.add(keyList);
			}
			if(currentSlice > 1)
				calcPersistence();
			else{
				MipavUtil.displayError("Not enough data to track persistence");
			}
		} else if(command.equals("End")){
			/*try {
				statsCSV.close();
			} catch (IOException e1) {
				MipavUtil.displayError("Could not close CSV output");
				e1.printStackTrace();
				return;
			}*/
			
							
			if (isExitRequired()) {
				System.exit(0);
				ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
			} else {
				if(subVolumeFrame != null) subVolumeFrame.close();
				if(imStack != null) imStack.disposeLocal();
				dispose();
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
	
	private void calcPersistence(){
		
		int sumSize = 0;
		
		for(int i=0;i<labelList.size();i++){
			sumSize += labelList.get(i).size();
		}
		
		if(sumSize == 0){
			MipavUtil.displayError("No branches to track. Please label branches.");
			return;
		}
		
		int max = 0;
		for(int i=0;i<labelList.size();i++){
			ArrayList<Integer> e = labelList.get(i);
			int thisMax = e.get(e.size()-1);
			if(thisMax > max)
				max = thisMax;
		}
		
		int[] persistence = new int[max];
		
		for(int i=0;i<labelList.size();i++){
			ArrayList<Integer> e = labelList.get(i);
			for(int j=0;j<e.size();j++){
				Integer num = e.get(j);
				persistence[num-1]++;
			}
		}
		
		String parent = swcList.get(0).getParent() + File.separator;
		String newCSV = parent + "persistence.csv";
		File csvFile = new File(newCSV);
		try {
			FileWriter statsCSV = new FileWriter(csvFile);
			String header = "Branch #,Persistence (Frames)\n";
			statsCSV.append(header);
			for(int i=0;i<max;i++){
				String out = String.format("%d,%d\n", i+1, persistence[i]);
				statsCSV.append(out);
			}
			
			statsCSV.append("\nFormat of is: Branch #: (X coordinate Y coordinate)\n\n");
			statsCSV.append("Time Point,Branch Coordinates\n");
			
			for(int i=0;i<hashList.size();i++){
				Hashtable<Integer, VOIPoint> table = hashList.get(i);
				ArrayList<Integer> e = labelList.get(i);
				String out = i + ",";
				for(int j=0;j<e.size();j++){
					int key = e.get(j);
					VOIPoint voi = table.get(key);
					Vector3f pt = voi.exportPoint();
					int ptx = (int)pt.X;
					int pty = (int)pt.Y;
					out += String.format("%d: (%d %d),", key, ptx, pty);
				}
				out += "\n";
				statsCSV.append(out);
			}
			
			statsCSV.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		
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
	
	/**
	 * Crude filter to get rid of shot noise resulting from the max-projected 
	 * neuron images. It implements a mean filter, but only adjusts pixels
	 * that are changing over a certain threshold. The mean filter is then
	 * repeated locally around points that were changed until there are no 
	 * longer any differences greater than the given threshold.
	 * 
	 * For 8-bit integer images, the threshold is set to 3, while for 16-bit
	 * integer images the threshold is 66. This does not work with float and
	 * double valued images right now.
	 * 
	 * This filter runs in place. 
	 * 
	 * @param image input image to filter. Is also the result image as this 
	 * filter places the results back into the original image
	 */
	
	private int[] filterShotNoiseMean(ModelImage image){
		int dataType = image.getType();
		int maxDiff;
		
		if(dataType == ModelImage.BYTE || dataType == ModelImage.UBYTE)
			maxDiff = 3;
		else if(dataType == ModelImage.SHORT || dataType == ModelImage.USHORT)
			maxDiff = 600;
		else return null;
		
		ModelImage meanImage = (ModelImage) image.clone();
		AlgorithmMean mean;
		if(imStack == null)
			mean = new AlgorithmMean(meanImage, 3, true);
		else
			mean = new AlgorithmMean(meanImage, 3, true, true);
		mean.run();
		int length = width*height;
		if(imStack != null) length *= numImages;
		int[] buffer = new int[length];
		int[] medBuffer = new int[length];
		int[] outBuffer = new int[length];
		int diff;
		
		
		try{
			image.exportData(0, length, buffer);
			meanImage.exportData(0, length, medBuffer);
		} catch(IOException e){
			MipavUtil.displayError("Could not export data from original image");
			e.printStackTrace();
		}
		
		ArrayList<Integer> adjustPts = new ArrayList<Integer>();
		ArrayList<Integer> addPts = new ArrayList<Integer>();
		for(int i=0;i<length;i++){
			diff = Math.abs(buffer[i] - medBuffer[i]);
			if(diff >= maxDiff){
				//adjustPts.add(i);
				buffer[i] = medBuffer[i];
				int x = i%width;
				int y = i/width;
				if(imStack != null) y = y%numImages;
				int z = imStack == null ? 0 : i/(width*height);
				for(int nx=x-1;nx<=x+1;nx++){
					if(nx<0 || nx>=width) continue;
					for(int ny=y-1;ny<=y+1;ny++){
						if(ny<0 || ny>=height) continue;
						int ind = nx+ny*width+z*(width*height);
						if(!adjustPts.contains(ind))
							adjustPts.add(ind);
					}
				}
			}
		}
		
		medBuffer = null;
		
		System.arraycopy(buffer, 0, outBuffer, 0, length);
		
		while(adjustPts.size()>0){
			int size = adjustPts.size();
			for(int j = 0;j<size;j++){
				int i = adjustPts.get(j);
				int x = i%width;
				int y = i/width;
				if(imStack != null) y = y%numImages;
				int z = imStack == null ? 0 : i/(width*height);
				int kMed = findMean(buffer, i);
				if(Math.abs(buffer[i] - kMed) >= maxDiff){
					outBuffer[i] = kMed;
					//adjustPts.add(i);
					for(int nx=x-1;nx<=x+1;nx++){
						if(nx<0 || nx>=width) continue;
						for(int ny=y-1;ny<=y+1;ny++){
							if(ny<0 || ny>=height) continue;
							int ind = nx+ny*width+z*(width*height);
							if(!addPts.contains(ind))
								addPts.add(ind);
						}
					}
				}
			}
			for(int j = 0;j<size;j++){
				int i=adjustPts.remove(0);
				buffer[i] = outBuffer[i];
			}
			adjustPts.addAll(addPts);
			addPts.clear();
		}
		
		meanImage.disposeLocal();
		return outBuffer;
	}
	
	private int findMean(int[] buffer, int i){
		int x = i%width;
		int y = i/width;
		if(imStack != null) y = y%numImages;
		int z = imStack == null ? 0 : i/(width*height);
		int kWidth = Math.min(3, 2 + Math.min(x, width-1-x));
		int kHeight = Math.min(3, 2 + Math.min(y, height-1-y));
		int cnt = kWidth*kHeight;
		int sum = 0;
	
		for(int nx=x-1;nx<=x+1;nx++){
			if(nx<0 || nx>=width) continue;
			for(int ny=y-1;ny<=y+1;ny++){
				if(ny<0 || ny>=height) continue;
				sum += buffer[nx+ny*width+z*width*height];
			}
		}
		int kMean = (int) ((float)sum / (float)cnt);
		return kMean;
	}
	
	/**
	 * Used with the primary constructor to allow the user to choose the
	 * directory where the images and SWC files are
	 */
	private void init(){
		
		getContentPane().removeAll();
		
		setForeground(Color.black);
        setTitle("Neuron Segmentation: Branch Persistence");

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
        
        /*JPanel csvPanel = new JPanel();
        csvPanel.setForeground(Color.black);
        
        csvBox = new JCheckBox("Delete previous stats CSV");
        csvBox.setFont(serif12);
        csvPanel.add(csvBox);*/
        
        JPanel noisePanel = new JPanel();
        noisePanel.setForeground(Color.black);
        
        noiseBox = new JCheckBox("Apply shot noise filter");
        noiseBox.setFont(serif12);
        noisePanel.add(noiseBox);
        
        PanelManager manage = new PanelManager();
        manage.add(choosePanel);
        manage.addOnNextLine(rangePanel);
        //manage.addOnNextLine(csvPanel);
        manage.addOnNextLine(noisePanel);
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

	private void initEditor(){
		
		getContentPane().removeAll();
		
		PanelManager manage = new PanelManager();
		ButtonGroup group = new ButtonGroup();
		
		JPanel descPanel = new JPanel();
		descPanel.setForeground(Color.black);
		descPanel.setBorder(buildTitledBorder("Editor Instructions"));
		String descStr = "<html><b>Choose add or delete. When add is selected,<br>"
				+ "double click a node to change the label to the displayed<br>"
				+ "branch value. When delete is selected, double click a<br>"
				+ "node to remove the label.</b></html>";
		
		JLabel descLabel = new JLabel(descStr);
		descLabel.setFont(serif12);
		descPanel.add(descLabel);
		
		getContentPane().add(descPanel, BorderLayout.NORTH);
		
		JPanel labelPanel = new JPanel();
		labelPanel.setForeground(Color.black);
		labelPanel.setBorder(buildTitledBorder("Current Image"));
		
		imName = new JLabel("Current Image");
		imName.setFont(serif12B);
		labelPanel.add(imName);

		manage.add(labelPanel);
		
		JPanel varsPanel = new JPanel(new GridLayout(0,2));
		
		JPanel radioPanel = new JPanel(new GridLayout(0,1));
        radioPanel.setForeground(Color.black);
        radioPanel.setBorder(buildTitledBorder("Node Editor"));
        
        addRB = new JRadioButton("Add Label");
        addRB.setFont(serif12);
        addRB.setSelected(true);
        radioPanel.add(addRB);
        group.add(addRB);
        
        deleteRB = new JRadioButton("Delete Label");
        deleteRB.setFont(serif12);
        radioPanel.add(deleteRB);
        group.add(deleteRB);
        varsPanel.add(radioPanel);
        //manage.addOnNextLine(radioPanel);
        
        JPanel spinnerPanel = new JPanel();
        spinnerPanel.setForeground(Color.black);
        spinnerPanel.setBorder(buildTitledBorder("Label Chooser"));
        
        JLabel spinnerLabel = new JLabel("Branch Label #");
        spinnerLabel.setFont(serif12);
        spinnerPanel.add(spinnerLabel);
        
        spinner = new JSpinner(new SpinnerListModel(spinnerList));
        spinner.setFont(serif12);
        spinner.setPreferredSize(new Dimension(40,20));
        
        spinnerPanel.add(spinner);

        varsPanel.add(spinnerPanel);

        //manage.addOnNextLine(spinnerPanel);
        manage.addOnNextLine(varsPanel);
		
		JPanel optionPanel = new JPanel(new GridLayout(0,3));
		optionPanel.setForeground(Color.black);
		optionPanel.setBorder(buildTitledBorder("Options"));
		
		/*JLabel noLabel = new JLabel("No options currently");
		noLabel.setFont(serif12);
		optionPanel.add(noLabel);*/
		
		disableBox = new JCheckBox("Hide trace");
		disableBox.setFont(serif12);
		disableBox.addItemListener(this);
		optionPanel.add(disableBox);
		
		hideVOIBox = new JCheckBox("Hide Points");
		hideVOIBox.setFont(serif12);
		hideVOIBox.addItemListener(this);
		optionPanel.add(hideVOIBox);
		
		/*saveBox = new JCheckBox("Save trace as image");
		saveBox.setFont(serif12);
		optionPanel.add(saveBox);
		
		deleteBox = new JCheckBox("Delete old SWC");
		deleteBox.setFont(serif12);
		optionPanel.add(deleteBox);*/
		
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
	
	private boolean initVars(){
		
		if(imStack == null){
			FileIO imReader = new FileIO();
			ModelImage sliceIm = imReader.readImage(images.get(0).getAbsolutePath());
			int[] extents = sliceIm.getExtents();
			width = extents[0];
			height = extents[1];
			length = width*height;
			
			sliceIm.disposeLocal();
		} else {
			int[] extents = imStack.getExtents();
			width = extents[0];
			height = extents[1];
			numImages = 1;
			if(extents.length > 2)
				numImages = extents[2];
			if(numImages == 1){
				MipavUtil.displayError("This image is not 3D");
				imStack = null;
				return false;
			}
			length = width*height;
			
			if(noiseBox.isSelected()){
				int[] buffer = filterShotNoiseMean(imStack);
				try{
					imStack.importData(0, buffer, true);
				} catch(IOException e){
					e.printStackTrace();
				}
			}
		}
		
		return true;
	}
	
	private void openHisto(){
		ViewJComponentEditImage comp = subVolumeFrame.getComponentImage();
		JFrameHistogram histoFrame = new JFrameHistogram(parentFrame, subVolume, null, comp.getLUTa(), null);
		histoFrame.histogramLUT(true, true);
	}
	
	private void openImage(){
		if(imStack == null){
			openImageFile();
		} else 
			openImageStack();
	}
	
	/**
	 * Opens up the new subvolume. To keep read times for the images low, it will
	 * only load images as necessary. It will use previous portions of the images
	 * as you go up and down the stack.
	 */
	private void openImageFile(){
		
		int lowerBound = Math.max(0, currentSlice - sliceRange);
		int upperBound = Math.min(numImages - 1, currentSlice + sliceRange);
		depth = upperBound - lowerBound + 1;
		int[] sliceBuffer = new int[length];
		
		FileIO imReader = new FileIO();
		ModelImage sliceIm = null;
		
		try{
			int type = ModelImage.USHORT;
			if(currentSlice == prevSlice){//Initial case
				imBuffer = new int[length*depth];
				for(int i=0;i<depth;i++){
					sliceIm = imReader.readImage(images.get(i+lowerBound).getAbsolutePath());
					if(noiseBox.isSelected())
						sliceBuffer = filterShotNoiseMean(sliceIm);
					else 
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
					if(noiseBox.isSelected())
						sliceBuffer = filterShotNoiseMean(sliceIm);
					else 
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
					if(noiseBox.isSelected())
						sliceBuffer = filterShotNoiseMean(sliceIm);
					else 
						sliceIm.exportData(0, length, sliceBuffer);
					System.arraycopy(sliceBuffer, 0, tempBuffer, copyDepth*length, length);
					sliceIm.disposeLocal();
				}
				imBuffer = tempBuffer;
			}
			
			activeSlice = currentSlice - lowerBound;

			imName.setText(images.get(activeSlice).getName());
			
			subVolume = new ModelImage(type, new int[]{width, height, depth}, "Sub-Volume");
			subVolume.importData(0, imBuffer, true);
			subVolumeFrame = new ViewJFrameImage(subVolume, lut);
			subVolumeFrame.setSlice(activeSlice);
			subVolumeFrame.setVisible(true);
			subVolumeFrame.addWindowListener(this);
			
			readSWC(lowerBound);
			
		} catch(IOException e){
			MipavUtil.displayError("Could not read SWC file(s)");
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
	
	private void openImageStack(){
		
		int lowerBound = Math.max(0, currentSlice - sliceRange);
		int upperBound = Math.min(numImages - 1, currentSlice + sliceRange);
		depth = upperBound - lowerBound + 1;
		
		String[] slices = new String[depth];
		for(int i=0;i<depth;i++){
			slices[i] = String.valueOf(lowerBound + i);
		}
		
		if(subVolume != null)
			subVolume.disposeLocal();
		
		subVolume = new ModelImage(ModelImage.USHORT, new int[]{width, height, depth}, "Sub-Volume");
		
		AlgorithmExtractSlices extract = new AlgorithmExtractSlices(imStack, subVolume, slices);
		extract.run();
		
		activeSlice = currentSlice - lowerBound;

		imName.setText("Current slice is: " + currentSlice);
		
		subVolumeFrame = new ViewJFrameImage(subVolume, lut);
		subVolumeFrame.setSlice(activeSlice);
		subVolumeFrame.setVisible(true);
		subVolumeFrame.addWindowListener(this);
		
		try {
			readSWC(lowerBound);
		} catch (IOException e) {
			e.printStackTrace();
		}

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
	private void readSWC(int lowerBound) throws IOException{
		
		//Reset things regarding the mask/control points here
		mask = new BitSet(length*depth);
		controlPts = new VOI((short) 0, "Control Points", VOI.POINT, 0);
		progenitorPt = null;
		progenitorVOI = null;
		voiHash = new Hashtable<Integer, VOIPoint>();
		
		ArrayList<String[]> points = null;
		String[] lineArray;
		
		for(int k=0;k<depth;k++){
			if(k+lowerBound >= swcList.size())
				continue;
			BufferedReader input = new BufferedReader(new FileReader(swcList.get(k + lowerBound)));
			String line = null; 
			points = new ArrayList<String[]>();
			while (( line = input.readLine()) != null){
				//Only need to recall information if we are reading
				//the active slice
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
					}
				}
				if(line.startsWith("#"))
					continue;
				lineArray = line.split(" ");
				if(lineArray.length > 0)
					points.add(lineArray);
			}
			input.close();
			
			int linkedTo;
			int x0, x1, y0, y1;
			String num;
			
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
				originVOI.setFixed(true);
			}
			
			for(int i=points.size()-1;i>0;i--){
				lineArray = points.get(i);
				num = lineArray[2];
				num = num.substring(0, num.indexOf("."));
				x0 = Integer.parseInt(num);
				num = lineArray[3];
				num = num.substring(0, num.indexOf("."));
				y0 = height - Integer.parseInt(num);
				
				if(k == activeSlice && lineArray[1].equals("6")){
					VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0,y0,activeSlice));
					voi.setLabel("");
					voi.setFixed(true);
					controlPts.importCurve(voi);
				}
				
				//Make the various line VOIs
				linkedTo = Integer.parseInt(lineArray[6]);

				lineArray = points.get(linkedTo - 1);
				num = lineArray[2];
				num = num.substring(0, num.indexOf("."));
				x1 = Integer.parseInt(num);
				num = lineArray[3];
				num = num.substring(0, num.indexOf("."));
				y1 = height - Integer.parseInt(num);

				bresenham(x0, y0, x1, y1, k);
			}
		}
		
		VOIBaseVector base = controlPts.getCurves();
		VOIPoint ptVOI;
		Vector3f ptVec;
		
		for(int i=0;i<base.size();i++){
			ptVOI = (VOIPoint)base.get(i);
			ptVec = ptVOI.exportPoint();
			Point checkPt = new Point((int)ptVec.X, (int)ptVec.Y);
			if(progenitorPt != null && progenitorPt.equals(checkPt)) {
				progenitorVOI = ptVOI;
				progenitorVOI.setLabel("P");
			} 
		}
		
		if(!hideVOIBox.isSelected()){
			subVolume.getVOIs().add(controlPts);
			subVolume.notifyImageDisplayListeners(null, true, 0, -1);
		}
		
		subVolumeFrame.getComponentImage().setPaintMask(mask);
		subVolumeFrame.getComponentImage().addMouseListener(this);
		subVolumeFrame.getComponentImage().addMouseMotionListener(this);
		
		float opacity = 1.0f;
		if(disableBox.isSelected())
			opacity = 0.0f;
		subVolumeFrame.getControls().getTools().setOpacity(opacity);
		subVolumeFrame.getControls().getTools().setPaintColor(Color.GREEN);
		subVolumeFrame.updateImages();
		
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
	
	private boolean populateImages(File dir){
		
		if(!dir.exists())
			return false;
		if(dir.isFile()){
			FileIO imReader = new FileIO();
			imStack = imReader.readImage(dir.getPath());
			
			String dirStr = dir.getParent();
			if(!dirStr.endsWith(File.separator))
				dirStr = dirStr + File.separator;
			dirStr = dirStr.concat("Branch_Images" + File.separator);
			
			FilenameFilter swcFilter = new FilenameFilter(){
				public boolean accept(File dir, String name) {
					return name.toLowerCase().endsWith(".swc");
				}
			};
			
			File[] files = (new File(dirStr)).listFiles(swcFilter);
			for(int i=0;i<files.length;i++){
				swcList.add(files[i]);
			}
			
			return true;
		}
		
		File skelName;
		String stripped;
		
		FilenameFilter imFilter = new FilenameFilter(){
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff")
						|| name.toLowerCase().endsWith(".lsm"));
			}
		};
		File[] files = dir.listFiles(imFilter);
		Comparator<File> fileComp = new Comparator<File>(){

			@Override
			public int compare(File o1, File o2) {
				String s1 = o1.getPath();
				String s2 = o2.getPath();
				if(s1.length() > s2.length())
					return 1;
				else if(s1.length() < s2.length())
					return -1;
				else{
					return Integer.signum(s1.compareTo(s2));
				}
			}
			
		};
		
		Arrays.sort(files, fileComp);
		
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
		
		return numImages > 0;
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		
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
		
		int thisSlice = subVolumeFrame.getViewableSlice();
		if(thisSlice != activeSlice)
			return;
		
		
		int numClicks = e.getClickCount();
		int button = e.getButton();
		
		if(numClicks != 2 || button != MouseEvent.BUTTON1)
			return;

		if(addRB.isSelected()){
			VOIBase activeVOI = lastActive;
			if(activeVOI == null) return;
			Integer label;
			try{
				spinner.commitEdit();
				Object val = spinner.getValue();
				if(val instanceof Integer){
					label = (Integer)spinner.getValue();
				} else {
					throw new NumberFormatException();
				}
			} catch (NumberFormatException ex){
				MipavUtil.displayError("Range is not an integer");
				return;
			} catch (ParseException ex) {
				MipavUtil.displayError("Could not read range value");
				ex.printStackTrace();
				return;
			}
			
			VOIPoint ptVOI = (VOIPoint) activeVOI;
			if(ptVOI.getLabel().equals("P")||
					ptVOI.getLabel().equals("O")){
				return;
			}
			if(!ptVOI.getLabel().equals("")){
				String strLabel = ptVOI.getLabel();
				Integer removeLabel = Integer.valueOf(strLabel);
				voiHash.remove(removeLabel);
			}
			VOIPoint removed = voiHash.remove(label);
			if(removed != null)
				removed.setLabel("");
			ptVOI.setLabel(label.toString());
			voiHash.put(label, ptVOI.clone());
			
			spinner.setValue(spinner.getNextValue());
			
			lastActive = null;
		} else if (deleteRB.isSelected()){
			VOIBase activeVOI = lastActive;
			if(activeVOI == null) return;
			String strLabel = activeVOI.getLabel();
			if(strLabel.equals("")) return;
			activeVOI.setLabel("");
			Integer label = Integer.valueOf(strLabel);
			voiHash.remove(label);
			lastActive = null;
		}
			
		subVolume.notifyImageDisplayListeners();
 
		controlPts.setAllActive(false);
		
	}

	/**
	 * The precursor to a drag movement involves a button
	 * press, so check to see if a VOI was clicked. If not,
	 * then any following move or delete actions will not
	 * occur. 
	 */
	@Override
	public void mousePressed(MouseEvent e) {
		
		VOIBaseVector pts = controlPts.getCurves();
		VOIBase activeVOI = null;
		
		for(int i=0;i<pts.size();i++){
			VOIBase current = pts.get(i);
			if(current.isActive()){
				activeVOI = current;
				break;
			}
		}
		
		lastActive = activeVOI;
	}

	/**
	 * Once the mouse is released, check to see if it was preceeded
	 * by a drag event, in which case you need to move whichever node
	 * was just dragged to another spot.
	 */
	@Override
	public void mouseReleased(MouseEvent e) {
		
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
			/*try {
				statsCSV.close();
			} catch (IOException e1) {
				MipavUtil.displayError("Could not close CSV output");
				e1.printStackTrace();
			}*/
			subVolumeFrame = null;
			actionPerformed(new ActionEvent(this, 0, "End"));
		}
	}
	
	public void itemStateChanged(ItemEvent e){
		Object source = e.getSource();
		if(source == disableBox){
			if(disableBox.isSelected()){
				subVolumeFrame.getControls().getTools().setOpacity(0.0f);
				subVolumeFrame.updateImages();
			} else{
				subVolumeFrame.getControls().getTools().setOpacity(1.0f);
				subVolumeFrame.getControls().getTools().setPaintColor(Color.GREEN);
				subVolumeFrame.updateImages();
			}
		} else if(source == hideVOIBox){
			if(hideVOIBox.isSelected()){
				subVolume.getVOIs().remove(controlPts);
				subVolume.notifyImageDisplayListeners(null, true, 0, -1);
			} else {
				if(!subVolume.getVOIs().contains(controlPts)){
					subVolume.getVOIs().add(controlPts);
					subVolume.notifyImageDisplayListeners(null, true, 0, -1);
				}
			}
		}
	}

}
