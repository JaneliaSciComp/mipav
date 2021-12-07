import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowEvent;
import java.util.BitSet;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.dialogs.JDialogBase;

/**
 * Dialog accompanying the neuron segmentation algorithm for the
 * Giniger lab. Once started, the algorithm will run the initial
 * segmentation and display it on the image as a paint mask. 
 * Once the initial segmentation is displayed, the user can then
 * add or delete branches off the neuron as they please. 
 * 
 * @author wangvg
 *
 */
public class PlugInDialogNeuronSegmentation extends JDialogBase implements
		AlgorithmInterface, MouseListener, ChangeListener {
	
	private static final long serialVersionUID = -5444231504112876834L;

	private JRadioButton addRB;
	
	private JCheckBox centroidBox;
	
	private JRadioButton changeRB;
	
	private int changeX;
	
	private int changeY;
	
	private JRadioButton deleteRB;
	
	private int[] extents;
	
	private ViewJFrameImage frame;
	
	private boolean listenersOn;
	
	private JCheckBox polygonalBox;
	
	private JCheckBox saveSkelBox;
	
	private JCheckBox saveVOIBox;
	
	private PlugInAlgorithmNeuronSegmentation seg;
	
	private ViewJFrameImage segFrame;
	
	private JCheckBox segImageBox;
	
	private JSlider sensSlider;
	
	private BitSet skeleton;
	
	private ModelImage srcImage;
	
	private JCheckBox tipBox;
	
	private JButton undoButton;
	
	private int width;

	public PlugInDialogNeuronSegmentation(ViewJFrameImage imFrame, ModelImage image){
		super();
		srcImage = image;
		extents = image.getExtents();
		width = extents[0];
		frame = imFrame;
		changeX = -1;
		changeY = -1;
		listenersOn = false;
		
		init();
		callAlgorithm();
		

	}
	
	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		
		if(command.equals("Undo") || command.equals("Redo"))
			undo();
		else if(command.equals("Save")){

			seg.save(saveVOIBox.isSelected(), saveSkelBox.isSelected());
		}
		else if(command.equals("End")){
			frame.getComponentImage().removeMouseListener(this);
			frame.removeWindowListener(this);
			finalize();
		}	
		else if(command.equals("Reset")){
			sensSlider.setValue(10);
			changeX = -1;
			changeY = -1;
			callAlgorithm();
		}
		else{
			super.actionPerformed(e);
		}
		
	}
	
	/**
	 * The algorithm's runAlgorithm() method provides the initial
	 * segmentation, so once that is performed, display it on the
	 * image as a paint mask, and set up the frame for later
	 * operations.
	 * 
	 * Also, initialize the dialog for the user to add/delete
	 * branches.
	 */
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		//Displays the skeleton as a paint mask for the image,
		//with opacity set to 1, and color set to white.
		skeleton = seg.getSkeleton();
		
		frame.getComponentImage().getImageA().resetVOIs();
		frame.getControls().getTools().setOpacity(1.0f);
		frame.getControls().getTools().setPaintColor(Color.WHITE);
		
		frame.setCursor(null);
		frame.getComponentImage().setPaintMask(skeleton);
		frame.updateImages();

		//Add mouse listener so you can click to add/delete branches
		if(!listenersOn){
			frame.getComponentImage().addMouseListener(this);
			frame.addWindowListener(this);
			listenersOn = true;
		}
		
		if(tipBox.isSelected()) seg.displayTips();
		if(centroidBox.isSelected()) seg.displayCentroid();
		if(polygonalBox.isSelected()) seg.displayPolygonal();
	}
	
	public void finalize(){
		seg.finalize();
		seg = null;
		dispose();
		skeleton = null;
	}
	
	protected void callAlgorithm(){
		//Run the initial segmentation on start-up so that
		//it is immediately displayed to the user
		
		seg = new PlugInAlgorithmNeuronSegmentation(srcImage);
		if(sensSlider == null){
			seg.setSensitivity(0.01f);
		}
		else{
			float sensitivity = 0.001f * (float)sensSlider.getValue();
	        if(sensitivity == 0) sensitivity = 1;
	        seg.setSensitivity(sensitivity);
		}
		seg.setCoords(changeX, changeY);
		createProgressBar("Segmenting Neuron", "Creating branches..." , seg);
		seg.addListener(this);
		if (isRunInSeparateThread()) {
			if (seg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			seg.run();
		}
	}
	
	private void init(){
		
		setForeground(Color.black);
        setTitle("Add/Delete Branches");
        
        JPanel descPanel = new JPanel();
        descPanel.setForeground(Color.black);
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Choose either add or delete, and then click on the image<br>"
        		+ "to modify the branches.<br>"
        		+ "Choose \"Change Location\" to change where the neuron is <br>"
        		+ "believed to be. <br>" 
        		+ "Change sensitivity to change original segmentation.<br><br>"
        		+ "<b>NOTE:</b> Changing sensitivity or location resets any <br>"
        		+ "branches added or deleted previously.</html>";
        
        JLabel descLabel = new JLabel(desc);
        descLabel.setForeground(Color.black);
        descLabel.setFont(serif12);
        descPanel.add(descLabel);
        
        JPanel radioPanel = new JPanel();
        radioPanel.setForeground(Color.black);
        
        addRB = new JRadioButton("Add");
        addRB.setFont(serif12);
        addRB.setSelected(true);
        
        deleteRB = new JRadioButton("Delete");
        deleteRB.setFont(serif12);
        
        changeRB = new JRadioButton("Change location");
        changeRB.setFont(serif12);
        
        ButtonGroup group = new ButtonGroup();
        
        group.add(addRB);
        group.add(deleteRB);
        group.add(changeRB);
        radioPanel.add(addRB);
        radioPanel.add(deleteRB);
        radioPanel.add(changeRB);
        
        JPanel titlePanel = new JPanel();
        titlePanel.setForeground(Color.black);
        
        JLabel slideLabel = new JLabel("Sensitivity", JLabel.CENTER);
        slideLabel.setFont(serif12B);
        titlePanel.add(slideLabel);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setForeground(Color.black);
        
        sensSlider = new JSlider(JSlider.HORIZONTAL, 0, 30, 10);
        sensSlider.addChangeListener(this);
        sensSlider.setMajorTickSpacing(5);
        sensSlider.setMinorTickSpacing(1);
        sensSlider.setFont(serif12);
        sensSlider.setPaintTicks(true);
        sensSlider.setPaintLabels(true);
        sliderPanel.add(sensSlider);
        
        JPanel checkPanel = new JPanel(new GridLayout(2,2));
        checkPanel.setForeground(Color.black);
        checkPanel.setBorder(buildTitledBorder("Display options"));
        
        centroidBox = new JCheckBox("Centroid");
        centroidBox.setFont(serif12);
        centroidBox.addItemListener(this);
        checkPanel.add(centroidBox);
        
        tipBox = new JCheckBox("Branch tips");
        tipBox.setFont(serif12);
        tipBox.addItemListener(this);
        checkPanel.add(tipBox);
        
        polygonalBox = new JCheckBox("Polygonal area");
        polygonalBox.setFont(serif12);
        polygonalBox.addItemListener(this);
        checkPanel.add(polygonalBox);
        
        segImageBox = new JCheckBox("Initial segmentation");
        segImageBox.setFont(serif12);
        segImageBox.addItemListener(this);
        checkPanel.add(segImageBox);
        
        JPanel optionsPanel = new JPanel(new GridLayout(0,2));
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Misc. Options"));
   
        saveSkelBox = new JCheckBox("Save branches as TIFF");
        saveSkelBox.setFont(serif12);
        saveSkelBox.setSelected(true);
        optionsPanel.add(saveSkelBox);
        
        saveVOIBox = new JCheckBox("Save VOIs");
        saveVOIBox.setFont(serif12);
        optionsPanel.add(saveVOIBox);
        
        PanelManager manage = new PanelManager();
        manage.add(radioPanel);
        manage.addOnNextLine(titlePanel);
        manage.addOnNextLine(sliderPanel);
        manage.addOnNextLine(checkPanel);
        manage.addOnNextLine(optionsPanel);
        
        getContentPane().add(descPanel, BorderLayout.NORTH);
        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);
        
        JPanel buttonPanel = new JPanel(new GridLayout(1,4));
        buttonPanel.setForeground(Color.black);
        
        JButton resetButton = new JButton("Reset");
        resetButton.setFont(serif12);
        resetButton.addActionListener(this);
        buttonPanel.add(resetButton);
        
        JButton saveButton = new JButton("Save");
        saveButton.setFont(serif12);
        saveButton.addActionListener(this);
        buttonPanel.add(saveButton);
        
        undoButton = new JButton("Undo");
        undoButton.setFont(serif12);
        undoButton.addActionListener(this);
        buttonPanel.add(undoButton);
        
        JButton endButton = new JButton("End");
        endButton.setFont(serif12);
        endButton.addActionListener(this);
        buttonPanel.add(endButton);
        
        
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
		
	}
	
	/**
	 * Very basic undo functionality is provided in the algorithm.
	 * This could also easily be implemented as only part of the
	 * dialog, which may be more useful for multi-undo/redo.
	 * 
	 * Also allows for redo.
	 */
	
	private void undo(){
		
		seg.undo();
		skeleton = seg.getSkeleton();
		if(skeleton == null){
			MipavUtil.displayError("No modification to undo");
			return;
		}
		
		frame.getComponentImage().setPaintMask(skeleton);
		frame.updateImages();
		if(undoButton.getText().equals("Undo"))
			undoButton.setText("Redo");
		else undoButton.setText("Undo");
		
		if(tipBox.isSelected()) seg.displayTips();
		if(centroidBox.isSelected()) seg.displayCentroid();
		if(polygonalBox.isSelected()) seg.displayPolygonal();
		
		
	}
	
	/**
	 * Toggles the various VOIs describing the
	 * neuron branches on or off
	 */
	
	public void itemStateChanged(ItemEvent e){
		
		Object source = e.getItemSelectable();
		
		if(source == tipBox){
			if(tipBox.isSelected()){
				seg.displayTips();
			}
			else seg.removeTips();
		}
		else if(source == centroidBox){
			if(centroidBox.isSelected()){
				seg.displayCentroid();
			}
			else seg.removeCentroid();
		}
		else if(source == polygonalBox){
			if(polygonalBox.isSelected()){
				seg.displayPolygonal();
			}
			else seg.removePolygonal();
		}
		else if(source == segImageBox){
			if(segImageBox.isSelected()){
				if(segFrame == null){
					ModelImage segImage = seg.getSegImage();
					segFrame = new ViewJFrameImage(segImage);
					segFrame.addWindowListener(this);
				}
				segFrame.setVisible(true);
			}
			else{
				if(segFrame != null){
					segFrame.setVisible(false);
				}
			}
		}
		
	}
	
	/**
	 * When the mouse is clicked on the image (ViewJComponentEditImage),
	 * branches are either added or deleted based on which
	 * radio button is currently selected.
	 */

	@Override
	public void mouseClicked(MouseEvent e) {
		
		float zoomX = frame.getComponentImage().getZoomX();
		float zoomY = frame.getComponentImage().getZoomY();
		int x = (int) ((float)e.getX()/zoomX); //- left;
		int y = (int) ((float)e.getY()/zoomY); //- top;

		int i = x + y*width;
		if(changeRB.isSelected()){
			changeX = x;
			changeY = y;
			callAlgorithm();
		}
		else {
			if(addRB.isSelected()) seg.addBranches(i);
			else seg.deleteBranches(i);
			
			skeleton = seg.getSkeleton();
			frame.getComponentImage().setPaintMask(skeleton);
			frame.updateImages();
			
			if(tipBox.isSelected()) seg.displayTips();
			if(centroidBox.isSelected()) seg.displayCentroid();
			if(polygonalBox.isSelected()) seg.displayPolygonal();
			
			undoButton.setText("Undo");
		}
		
	}
	
	//We only care about the click for determining
	//where to see adds or deletions

	@Override
	public void mousePressed(MouseEvent e) {
		// Do nothing
		
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		// Do nothing
		
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		// Do nothing
		
	}

	@Override
	public void mouseExited(MouseEvent e) {
		// Do nothing
		
	}
	
	//Make sure if image window closes, so does the dialog.
	@Override
	public void windowClosing(WindowEvent event) {
		ViewJFrameImage source = (ViewJFrameImage) event.getSource();
		if(source == segFrame){
			segFrame = null;
			segImageBox.setSelected(false);
			return;
		}
		else if(source != frame){
			frame.getComponentImage().removeMouseListener(this);
			frame.removeWindowListener(this);
		}
        cancelFlag = true;
        finalize();
    }
	
	/**
	 * Allow the user to change how sensitive the initial segmentation
	 * is. This essentially reruns the initial segmentation step, but
	 * with a different threshold to build the skeleton.
	 */

	@Override
	public void stateChanged(ChangeEvent e) {

		JSlider source = (JSlider)e.getSource();
	    if (!source.getValueIsAdjusting()) {
	    	callAlgorithm();/*
	        float sensitivity = 0.001f * (float)sensSlider.getValue();
	        if(sensitivity == 0) sensitivity = 1;
	        seg.setSensitivity(sensitivity);
	        createProgressBar("Segmenting Neuron", "Creating branches..." , seg);
	        seg.runAlgorithm();
			
			skeleton = seg.getSkeleton();
			frame.getComponentImage().setPaintMask(skeleton);
			frame.updateImages();
			
			if(tipBox.isSelected()) seg.displayTips();
			if(centroidBox.isSelected()) seg.displayCentroid();
			if(polygonalBox.isSelected()) seg.displayPolygonal();
	        */
	    }
	}

}
