import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.JTextPane;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

/**
 * A sister plugin to the 3DSWCStats set of plugins. This dialog opens up
 * a very rudimentary 3D viewer of the neuron skeleton. The user can
 * then choose which branch to use as the axon when exported to a SWC
 * and in the stats CSV. 
 * @see PlugInDialog3DSWCStats
 * @author wangvg
 *
 */

public class PlugInDialog3DSWCViewer extends JDialogBase implements
		AlgorithmInterface, ChangeListener, ListSelectionListener, 
		MouseListener, MouseMotionListener, MouseWheelListener{

	/**
	 * 
	 */
	private static final long serialVersionUID = 4712836845621801009L;

	private PlugInAlgorithm3DSWCViewer alg;

	private SimpleAttributeSet attr;
	
	private JRadioButton axonRB;

	private int buttonPressed;

	private ViewJFrameImage frame;

	private Point prevPt;

	private String resUnit;

	/**
	 * Sliders to control rotation about the
	 * x-, y-, and z-axes
	 */
	private JSlider[] sliders;

	/**
	 * Spinners to control rotation about the
	 * x-, y-, and z-axes while also displaying
	 * the value of each
	 */
	private JSpinner[] spinners;

	private File swcFile;
	
	private JTextPane textArea;
	
	@SuppressWarnings("rawtypes")
	/**
	 * List used to display and select which
	 * branch to denote as the axon
	 */
	private JList tips;
	
	private JRadioButton showAxonRB;
	
	private String imageFile;
	
	public PlugInDialog3DSWCViewer(String imFile, File file, JTextPane text, String unit){
		
		imageFile = imFile;
		swcFile = file;
		textArea = text;
		resUnit = unit;
		
		attr = new SimpleAttributeSet();
		StyleConstants.setFontFamily(attr, "Serif");
		StyleConstants.setFontSize(attr, 12);

		try {
			textArea.getDocument().remove(0, textArea.getDocument().getLength());
		} catch (BadLocationException e) {}
		
		append("Creating viewer...", attr);
		
		init();
		
		setup();
		
	}
	
	public PlugInDialog3DSWCViewer(JTextPane text, String unit, PlugInAlgorithm3DSWCViewer algorithm){
		
		alg = algorithm; //yes this is a bit backwards but it'll work
		alg.addListener(this);
		
		textArea = text;
		resUnit = unit;
		
		attr = new SimpleAttributeSet();
		StyleConstants.setFontFamily(attr, "Serif");
		StyleConstants.setFontSize(attr, 12);

		/*try {
			textArea.getDocument().remove(0, textArea.getDocument().getLength());
		} catch (BadLocationException e) {}*/
		
		append("Creating viewer...", attr);
		
		init();
		
	}
	
	public void actionPerformed(ActionEvent event){
		
		String command = event.getActionCommand();
		if(command.equals("ok")){
			setVisible(false);
			frame.close();
			
			append("Closing viewer...", attr);
			append("Writing with new axon choice", attr);
			alg.setUseLength(axonRB.isSelected());
			alg.write();
		}else if(command.equals("cancel")){
			alg.viewerClosed();
			alg.setCompleted(false);
			alg.notifyListeners(alg);
			append("Closing viewer...", attr);
			append("-----------------------------------------", attr);
			frame.close();
			dispose();
		}else if(command.equals("axon")){
			alg.showAxon();
			Object obj = tips.getSelectedValue();
			if(obj instanceof String){
				String label = obj.toString();
				String num = label.split(" ")[1];
				Integer branch;
				try{
					branch = Integer.valueOf(num);
				} catch (NumberFormatException ne){
					//This should never happen but on the off-chance something goes wrong
					SimpleAttributeSet redText = new SimpleAttributeSet(attr);
					StyleConstants.setForeground(redText, Color.red.darker());
					append("Invalid branch choice. Check for non-integer numbers.", redText);
					return;	
				}
				BitSet axonMask = alg.highlightAxon(branch);
				
				frame.getComponentImage().setPaintMask(axonMask);
				frame.getControls().getTools().setPaintColor(Color.RED);
				frame.updateImages(true);
			}
		}else if(command.equals("hull")){
			alg.showHull();
			BitSet hullMask = alg.convexHull();
			frame.getComponentImage().setPaintMask(hullMask);
			frame.getControls().getTools().setPaintColor(Color.GREEN);
			frame.updateImages(true);
			
		}else{
			super.actionPerformed(event);
		}
		
	}
	
	@SuppressWarnings("unchecked")
	@Override
	/**
	 * Only performed after the Imaris file has
	 * been imported and the general SWC structure
	 * has been setup for display. Populates the list
	 * of potential filaments to use as the axon and
	 * selects the most likely one to start with. 
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof PlugInAlgorithm3DSWCViewer){
			if(algorithm.isCompleted()){
				algorithm.setCompleted(false);
				algorithm.removeListener(this);
				frame = new ViewJFrameImage(alg.getDestImage());
				ArrayList<Integer> tipList = alg.getTips();
				Vector<String> tipName = new Vector<String>();
				for(Integer i : tipList){
					String name = "Filament " + i.toString();
					tipName.add(name);
				}
				tips.setListData(tipName);
				tips.setSelectedIndex(0);
				BitSet axonMask = alg.highlightAxon(tipList.get(0));
				frame.getComponentImage().setPaintMask(axonMask);
				//BitSet hullMask = alg.convexHull();
				//frame.getComponentImage().setPaintMask(hullMask);
				frame.getControls().getTools().setOpacity(1.0f);
				frame.getControls().getTools().setPaintColor(Color.RED);
				frame.addWindowListener(this);
				frame.setVisible(true);
				
				ViewJComponentEditImage comp = frame.getComponentImage();
				comp.removeMouseListener(comp);
				comp.removeMouseMotionListener(comp);
				comp.removeMouseWheelListener(comp);
				comp.addMouseListener(this);
				comp.addMouseMotionListener(this);
				comp.addMouseWheelListener(this);
				
	
				pack();
				setVisible(true);
				System.gc();
				
			}else{
				MipavUtil.displayError("Could not build viewer. Check"
						+ "debugging output for more information.");
			}
		}
		
	}
	
	private void append(String message, AttributeSet a){
		Document doc = textArea.getDocument();
		try {
			doc.insertString(doc.getLength(), message + "\n", a);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		
		textArea.setCaretPosition(doc.getLength());
	}
	
	@SuppressWarnings("rawtypes")
	private void init(){
		
		setTitle("3D Neuron Viewer");
		
		getContentPane().removeAll();
		
		getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.PAGE_AXIS));
		
		sliders = new JSlider[6];
		spinners = new JSpinner[6];
		//This is order of sliders/spinners
		String[] labelStr = new String[]{"Tx", "Ty", "Rx", "Ry", "Rz", "Zoom"};
		JLabel[] labels = new JLabel[6];
		
		for(int i=0;i<5;i++){
			if(i<2){
				sliders[i] = new JSlider(JSlider.HORIZONTAL, -500, 500, 0);
				spinners[i] = new JSpinner(new SpinnerNumberModel(0, -1000, 1000, 1));
			}
			else{
				sliders[i] = new JSlider(JSlider.HORIZONTAL, -180, 180, 0);
				spinners[i] = new JSpinner(new SpinnerNumberModel(0, -180, 180, 1));
			}
			sliders[i].setFont(serif12);
			sliders[i].addChangeListener(this);
			
			spinners[i].setFont(serif12);
			spinners[i].addChangeListener(this);
			labels[i] = new JLabel(labelStr[i]);
			labels[i].setFont(serif12B);
		}
		
		sliders[5] = new JSlider(JSlider.HORIZONTAL, 1, 100, 10);
		sliders[5].setPaintTicks(false);
		sliders[5].setFont(serif12);
		sliders[5].addChangeListener(this);
		spinners[5] = new JSpinner(new SpinnerNumberModel(1.0, 0.1, 10.0, 0.1));
		spinners[5].setFont(serif12);
		spinners[5].addChangeListener(this);
		labels[5] = new JLabel("Zoom");
		labels[5].setFont(serif12B);
		
		JPanel topPanel = new JPanel(new GridBagLayout());
		
		topPanel.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Change view"));
		topPanel.setForeground(Color.black);
		
		GridBagConstraints gbc = new GridBagConstraints();
	
		gbc.insets = new Insets(0, 5, 0, 5);
		gbc.gridwidth = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		
		for(int i=0;i<6;i++){
			gbc.gridy = i;
			gbc.gridx = 0;
			gbc.anchor = GridBagConstraints.EAST;
			topPanel.add(labels[i], gbc);
			gbc.gridx = 1;
			gbc.gridwidth = 3;
			gbc.anchor = GridBagConstraints.WEST;
			topPanel.add(sliders[i], gbc);
			gbc.gridx = 4;
			gbc.gridwidth = 1;
			topPanel.add(spinners[i], gbc);
		}
		
		//getContentPane().add(topPanel, BorderLayout.NORTH);
		getContentPane().add(topPanel);
		
		JPanel overlayPanel = new JPanel();
		overlayPanel.setForeground(Color.black);
		overlayPanel.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Overlay Options"));
		overlayPanel.setLayout(new GridLayout(0,2));
		
		ButtonGroup overlayGroup = new ButtonGroup();
		
		showAxonRB = new JRadioButton("Highlight Axon");
		showAxonRB.setFont(serif12);
		showAxonRB.setActionCommand("axon");
		showAxonRB.addActionListener(this);
		showAxonRB.setSelected(true);
		overlayGroup.add(showAxonRB);
		overlayPanel.add(showAxonRB);
		
		JRadioButton showHullRB = new JRadioButton("Display Hull");
		showHullRB.setFont(serif12);
		showHullRB.setActionCommand("hull");
		showHullRB.addActionListener(this);
		overlayGroup.add(showHullRB);
		overlayPanel.add(showHullRB);
		
		getContentPane().add(overlayPanel);
		
		
		tips = new JList();
		tips.addListSelectionListener(this);
		tips.setFont(serif12);
		tips.setVisibleRowCount(10);
		
		JScrollPane scrollPane = new JScrollPane(tips);
		scrollPane.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Select axon"));
		
		JPanel middlePanel = new JPanel(new BorderLayout());
		middlePanel.setForeground(Color.black);
		middlePanel.setBorder(new EmptyBorder(0,0,0,0));
		middlePanel.add(scrollPane, BorderLayout.CENTER);
		
		JPanel rbPanel = new JPanel(new GridLayout(0,2));
		rbPanel.setForeground(Color.black);
		rbPanel.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Branch ordering"));
		
		ButtonGroup group = new ButtonGroup();
		
		axonRB = new JRadioButton("Use absolute length");
		axonRB.setFont(serif12);
		axonRB.setSelected(true);
		group.add(axonRB);
		
		JRadioButton imarisRB = new JRadioButton("Infer from file");
		imarisRB.setFont(serif12);
		group.add(imarisRB);
		
		rbPanel.add(axonRB);
		rbPanel.add(imarisRB);
		
		middlePanel.add(rbPanel, BorderLayout.SOUTH);
		
		//getContentPane().add(middlePanel, BorderLayout.CENTER);
		getContentPane().add(middlePanel);
		
		JPanel bottomPanel = new JPanel();
		bottomPanel.setForeground(Color.black);
		
		buildOKCancelButtons();
		
		OKButton.setActionCommand("ok");
		cancelButton.setActionCommand("cancel");
		
		bottomPanel.add(OKButton);
		bottomPanel.add(cancelButton);
		
		//getContentPane().add(bottomPanel, BorderLayout.SOUTH);
		getContentPane().add(bottomPanel);
		
		
	}

	/**
	 * Run the setup step for the algorithm, which
	 * just reads the Imaris file and makes some
	 * basic inferences. 
	 */
	private void setup(){
		alg = new PlugInAlgorithm3DSWCViewer(imageFile, swcFile, textArea, resUnit, axonRB.isSelected(), true);
		alg.addListener(this);
		if(isRunInSeparateThread()){
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
	}
	
	@Override
	/**
	 * Constantly rotate the projection when the sliders
	 * or spinners change. Also update the sliders when
	 * the spinners change, and vice versa. 
	 */
	public void stateChanged(ChangeEvent e) {
		if(e.getSource() instanceof JSlider){
			int ind;
			for(ind=0;ind<sliders.length;ind++){
				if(sliders[ind] == e.getSource())
					break;
			}
			
			int value = sliders[ind].getValue();
			if(ind == 5)
				spinners[ind].setValue((double)value/10.0);
			else
				spinners[ind].setValue(value);
			
			int tx = sliders[0].getValue();
			int ty = sliders[1].getValue();
			int rx = sliders[2].getValue();
			int ry = sliders[3].getValue();
			int rz = sliders[4].getValue();
			double zoom = (double)sliders[5].getValue() / 10.0;
			
			alg.transformImage(tx, ty, rx, ry, rz, zoom);
			
		} else if(e.getSource() instanceof JSpinner){
			int ind;
			for(ind=0;ind<spinners.length;ind++){
				if(spinners[ind] == e.getSource())
					break;
			}
			
			SimpleAttributeSet redText = new SimpleAttributeSet(attr);
			StyleConstants.setForeground(redText, Color.red.darker());
			
			try{
				spinners[ind].commitEdit();
				Object val = spinners[ind].getValue();
				if(ind < 5){
					int value;
					if(val instanceof Integer){
						value = (Integer)spinners[ind].getValue();
					} else {
						throw new NumberFormatException();
					}
					sliders[ind].setValue(value);
				}else if(ind == 5){
					double value;
					if(val instanceof Double){
						value = (Double)spinners[ind].getValue();
					} else {
						throw new NumberFormatException();
					}
					int intVal = (int) (value*10);
					sliders[ind].setValue(intVal);
				}
			} catch (NumberFormatException ne){
				MipavUtil.displayError("Could not format a value into a number");
				append("Error in formatting values.", redText);
				append(e.toString(), redText);
				for(StackTraceElement t : ne.getStackTrace())
					append(t.toString(), redText);
				return;
			} catch (ParseException pe) {
				MipavUtil.displayError("Could not parse a value");
				append("Error in parsing values.", redText);
				append(e.toString(), redText);
				for(StackTraceElement t : pe.getStackTrace())
					append(t.toString(), redText);
				return;
			}
			
			int tx = sliders[0].getValue();
			int ty = sliders[1].getValue();
			int rx = sliders[2].getValue();
			int ry = sliders[3].getValue();
			int rz = sliders[4].getValue();
			double zoom = (double)sliders[5].getValue() / 10.0;
			
			alg.transformImage(tx, ty, rx, ry, rz, zoom);
		}
	}

	@Override
	/**
	 * Highlight a branch whenever the user selects 
	 * a filament from the list. 
	 */
	public void valueChanged(ListSelectionEvent e) {
		if(showAxonRB.isSelected()){
			Object obj = tips.getSelectedValue();
			if(obj instanceof String){
				String label = obj.toString();
				String num = label.split(" ")[1];
				Integer branch;
				try{
					branch = Integer.valueOf(num);
				} catch (NumberFormatException ne){
					//This should never happen but on the off-chance something goes wrong
					SimpleAttributeSet redText = new SimpleAttributeSet(attr);
					StyleConstants.setForeground(redText, Color.red.darker());
					append("Invalid branch choice. Check for non-integer numbers.", redText);
					return;	
				}
				BitSet axonMask = alg.highlightAxon(branch);
				
				frame.getComponentImage().setPaintMask(axonMask);
				frame.updateImages(true);
				/*frame.getControls().getTools().setOpacity(1.0f);
				frame.getControls().getTools().setPaintColor(Color.RED);
				frame.setVisible(true);*/
			}
		}
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		
	}

	@Override
	public void mousePressed(MouseEvent e) {
		int x = e.getX();
		int y = e.getY();
		prevPt = new Point(x, y);
		buttonPressed = e.getButton();
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

	@Override
	public void mouseDragged(MouseEvent e) {
		
		int x = e.getX();
		int y = e.getY();
		
		Point currPt = new Point(x, y);
		
		int diffX = x - prevPt.x;
		int diffY = y - prevPt.y;
		
		int tx = sliders[0].getValue();
		int ty = sliders[1].getValue();
		double zoom = (double)sliders[5].getValue()/10.0;
		
		
		if(buttonPressed == MouseEvent.BUTTON1){
			
			int[] ra = alg.mouseRotate(diffY, diffX);
			
			for(int i=0;i<6;i++){
				sliders[i].removeChangeListener(this);
				spinners[i].removeChangeListener(this);
			}
			
			for(int i=2;i<5;i++){
				sliders[i].setValue(ra[i-2]);
				spinners[i].setValue(ra[i-2]);
			}	
			for(int i=0;i<6;i++){
				sliders[i].addChangeListener(this);
				spinners[i].addChangeListener(this);
			}
			
			alg.transformImage(tx, ty, ra[0], ra[1], ra[2], zoom);
			
		}else if(buttonPressed == MouseEvent.BUTTON3){
			
			alg.mouseTranslate(diffX, diffY, zoom);
			
			for(int i=0;i<6;i++){
				sliders[i].removeChangeListener(this);
				spinners[i].removeChangeListener(this);
			}
			
			sliders[0].setValue(tx + diffX);
			sliders[1].setValue(ty + diffY);
			spinners[0].setValue(tx + diffX);
			spinners[1].setValue(ty + diffY);
			
			for(int i=0;i<6;i++){
				sliders[i].addChangeListener(this);
				spinners[i].addChangeListener(this);
			}
		}else{
			return;
		}
		
		prevPt = currPt;
		
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		
	}
	
	public void windowClosing(WindowEvent e){
		if(e.getSource() == this ||
				e.getSource() == frame){
			actionPerformed(new ActionEvent(this, 0, "cancel"));
		}
	}

	@Override
	public void mouseWheelMoved(MouseWheelEvent e) {
		
		int num = e.getWheelRotation();
		int value = sliders[5].getValue();
		sliders[5].setValue(value - num);
		
		
	}
	
	

	
	
}
