import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.JTextPane;
import javax.swing.SpinnerNumberModel;
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
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialog3DSWCViewer extends JDialogBase implements
		AlgorithmInterface, ChangeListener, ListSelectionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4712836845621801009L;

	private File swcFile;
	
	private JTextPane textArea;
	
	private JSlider[] sliders;
	
	private JSpinner[] spinners;
	
	@SuppressWarnings("rawtypes")
	private JList tips;
	
	private PlugInAlgorithm3DSWCViewer alg;
	
	private ViewJFrameImage frame;
	
	private SimpleAttributeSet attr;
	
	private String resUnit;
	
	public PlugInDialog3DSWCViewer(File file, JTextPane text, String unit){
		
		swcFile = file;
		textArea = text;
		resUnit = unit;
		
		attr = new SimpleAttributeSet();
		StyleConstants.setFontFamily(attr, "Serif");
		StyleConstants.setFontSize(attr, 12);

		append("Creating viewer...", attr);
		
		init();
		
		setup();
		
	}
	
	public void actionPerformed(ActionEvent event){
		
		String command = event.getActionCommand();
		if(command.equals("ok")){
			setVisible(false);
			frame.close();
			append("Closing viewer...", attr);
			append("Writing with new axon choice", attr);
			alg.write();
		}else if(command.equals("cancel")){
			append("Closing viewer...", attr);
			frame.close();
			dispose();
		}else{
			super.actionPerformed(event);
		}
		
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof PlugInAlgorithm3DSWCViewer){
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
			frame.getControls().getTools().setOpacity(1.0f);
			frame.getControls().getTools().setPaintColor(Color.RED);
			frame.setVisible(true);

			pack();
			setVisible(true);
			System.gc();
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
	
	private void setup(){
		alg = new PlugInAlgorithm3DSWCViewer(swcFile, textArea, resUnit);
		alg.addListener(this);
		if(isRunInSeparateThread()){
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
	}
	
	@SuppressWarnings("rawtypes")
	private void init(){
		
		sliders = new JSlider[3];
		spinners = new JSpinner[3];
		String[] labelStr = new String[]{"X", "Y", "Z"};
		JLabel[] labels = new JLabel[3];
		
		for(int i=0;i<3;i++){
			sliders[i] = new JSlider(JSlider.HORIZONTAL, 0, 360, 0);
			sliders[i].setFont(serif12);
			sliders[i].addChangeListener(this);
			spinners[i] = new JSpinner(new SpinnerNumberModel(0, 0, 360, 1));
			spinners[i].setFont(serif12);
			spinners[i].addChangeListener(this);
			labels[i] = new JLabel(labelStr[i]);
			labels[i].setFont(serif12B);
		}
		
		JPanel topPanel = new JPanel(new GridBagLayout());
		topPanel.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Rotate image"));
		topPanel.setForeground(Color.black);
		
		GridBagConstraints gbc = new GridBagConstraints();

		gbc.insets = new Insets(0, 5, 0, 5);
		gbc.gridwidth = 1;
		
		for(int i=0;i<3;i++){
			gbc.gridy = i;
			gbc.gridx = 0;
			topPanel.add(labels[i], gbc);
			gbc.gridx = 1;
			gbc.gridwidth = 3;
			topPanel.add(sliders[i], gbc);
			gbc.gridx = 4;
			gbc.gridwidth = 1;
			topPanel.add(spinners[i], gbc);
		}
		
		getContentPane().add(topPanel, BorderLayout.NORTH);
		
		tips = new JList();
		tips.addListSelectionListener(this);
		tips.setFont(serif12);
		tips.setVisibleRowCount(10);
		
		JScrollPane scrollPane = new JScrollPane(tips);
		
		JPanel middlePanel = new JPanel(new BorderLayout());
		middlePanel.setForeground(Color.black);
		middlePanel.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Select axon"));
		middlePanel.add(scrollPane, BorderLayout.CENTER);
		
		getContentPane().add(middlePanel, BorderLayout.CENTER);
		
		JPanel bottomPanel = new JPanel();
		bottomPanel.setForeground(Color.black);
		
		buildOKCancelButtons();
		
		OKButton.setActionCommand("ok");
		cancelButton.setActionCommand("cancel");
		
		bottomPanel.add(OKButton);
		bottomPanel.add(cancelButton);
		
		getContentPane().add(bottomPanel, BorderLayout.SOUTH);
		
		
	}

	@Override
	public void stateChanged(ChangeEvent e) {
		if(e.getSource() instanceof JSlider){
			int ind;
			for(ind=0;ind<sliders.length;ind++){
				if(sliders[ind] == e.getSource())
					break;
			}
			
			int value = sliders[ind].getValue();
			spinners[ind].setValue(value);
			
			int rx = sliders[0].getValue();
			int ry = sliders[1].getValue();
			int rz = sliders[2].getValue();
			
			alg.rotateImage(rx, ry, rz);
			
		} else if(e.getSource() instanceof JSpinner){
			int ind;
			for(ind=0;ind<spinners.length;ind++){
				if(spinners[ind] == e.getSource())
					break;
			}
			
			try{
				spinners[ind].commitEdit();
				Object val = spinners[ind].getValue();
				int value;
				if(val instanceof Integer){
					value = (Integer)spinners[ind].getValue();
				} else {
					throw new NumberFormatException();
				}
				sliders[ind].setValue(value);
			} catch (NumberFormatException ne){
				MipavUtil.displayError("Rotation value is not an integer");
				return;
			} catch (ParseException pe) {
				MipavUtil.displayError("Could not read range value");
				pe.printStackTrace();
			}
			
			int rx = sliders[0].getValue();
			int ry = sliders[1].getValue();
			int rz = sliders[2].getValue();
			
			alg.rotateImage(rx, ry, rz);
		}
	}

	@Override
	public void valueChanged(ListSelectionEvent e) {
		Object obj = tips.getSelectedValue();
		if(obj instanceof String){
			String label = obj.toString();
			String num = label.split(" ")[1];
			Integer branch = Integer.valueOf(num);
			BitSet axonMask = alg.highlightAxon(branch);
			
			frame.getComponentImage().setPaintMask(axonMask);
			frame.getControls().getTools().setOpacity(1.0f);
			frame.getControls().getTools().setPaintColor(Color.RED);
			frame.setVisible(true);
		}
	}
	
	

	
	
}
