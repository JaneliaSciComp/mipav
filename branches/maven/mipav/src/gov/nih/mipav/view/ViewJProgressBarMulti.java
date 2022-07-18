package gov.nih.mipav.view;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ProgressChangeEvent;
import gov.nih.mipav.view.ViewJProgressBar;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowListener;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.*;

/**
 * Extension of ViewJProgressBar, ViewJProgressBarMulti is used to keep track of multiple
 * algorithms or threads at a single time. Each algorithm/thread must be registered to the
 * multi-bar, or linked in AlgorithmBase. A progress bar will update as progress changes are
 * fired. This class acts as like a bunch of different ViewJProgressBars, except in one frame
 * instead of many. 
 * 
 * Unlike ViewJProgressBar, the frame will not automatically close. Even when all registered
 * threads are completed, the frame will remain visible. 
 * 
 * @author wangvg
 *
 */
public class ViewJProgressBarMulti extends ViewJProgressBar implements MouseListener{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1298459723854411415L;
	
	/**
	 * Panel containing all the progress bars as well as relevent
	 * text labels
	 */
	private JPanel barPanel;

	/**
	 * List containing which runnables are registered to this listener
	 */
	private ArrayList<Runnable> algList;

	private boolean messageLocked = false;

	private ActionListener actionListener;

	private WindowListener windowListener;

	private boolean cancelFlag;

	private JButton cancelButton;

	/**
	 * Label to display when there are no runnables to display or
	 * listen for. 
	 */
	private JLabel emptyLabel = new JLabel("No threads registered");

	/**
	 * Whether or not emptyLabel is currently displayed
	 */
	private boolean emptyDisplayed;

	private GridBagConstraints gbc;

	/**
	 * List to determine which progress bar to hide/show
	 */
	private ArrayList<JLabel> hideList; 

	/**
	 * List to determine which progress bar to close
	 */
	private ArrayList<JLabel> closeList;
	
	/**
	 * List of containers that hold all the information to correctly
	 * draw each progress bar 
	 */
	private ArrayList<AlgoContainer> algCont;

	/**
	 * Constructor to start building the frame. It mostly only needs the title 
	 * for the frame. The other paramters are just being carried over from the
	 * single version.
	 * 
	 * @param _title title of the frame
	 * @param _cancelFlag
	 * @param _actionListener
	 * @param _windowListener
	 */
	public ViewJProgressBarMulti(String _title, boolean _cancelFlag,
			ActionListener _actionListener, WindowListener _windowListener) {

		super(_title, null, 0, 100, _cancelFlag, _actionListener, _windowListener, false);

		algList = new ArrayList<Runnable>();
		hideList = new ArrayList<JLabel>();
		closeList = new ArrayList<JLabel>();
		algCont = new ArrayList<AlgoContainer>();

		windowListener = _windowListener;
		actionListener = _actionListener;
		cancelFlag = _cancelFlag;
		cancelButton = getCancelButton();

		gbc = new GridBagConstraints();
		gbc.gridy = 0;

		emptyLabel.setPreferredSize(new Dimension(270,45));

		getContentPane().removeAll();
		init();
	}

	/**
	 * Basic method for registering an algorithm to the multi-bar. Should be
	 * called in conjunction with addPropertyChangeListener (in AlgorithmBase)
	 * if the algorithm is an instance of AlgorithmBase.
	 * 
	 * @param alg the algorithm to register to the multi-bar
	 * @param title the name to associate with this algorithm
	 * @param min min progress value. Should be set to 0.
	 * @param max max progress value. Should be set to 100.
	 */
	public void addRunnable(Runnable alg, String title, int min, int max){
		
		AlgoContainer container = new AlgoContainer(title, min,max);
		algCont.add(container);
		algList.add(alg);
	}

	/**
	 * Secondary method for registering algorithms to the multi-bar. The user can
	 * pass a collection of algorithms (which must be accompanied by a title) to
	 * register to the multi-bar. Each algorithm should already have added this
	 * as its property change listener. 
	 * 
	 * @param algs the collection of algorithms to register to the multi-bar
	 * @param titles the collection of titles to assocaite with each algorithm
	 */
	public void addCollectionOfRunnable(Collection<Runnable> algs, Collection<String> titles){
		
		if(algs.size() !=  titles.size()){
			System.err.println("Number of messages does not match with number of threads");
			return;
		}
		algList.addAll(algs);
		
		Iterator<String> titleIter = titles.iterator();
		Iterator<Runnable> algIter = algs.iterator();
		
		Runnable alg;
		String title;
		AlgoContainer container;
		
		for(int i=0;i<algs.size();i++){
			alg = algIter.next();
			algList.add(alg);
			title = titleIter.next();
			container = new AlgoContainer(title, 0, 100);
			algCont.add(container);
			hideList.add(container.hideLabel);
			closeList.add(container.closeLabel);
		}
	}

	/**
	 * Handle any progress changes by updating the correct progress bar
	 * attached to the throwing algorithm
	 */
	public void progressStateChanged(ProgressChangeEvent e) {

		int value = e.getValue();
		Object src = e.getSource();

		int index = algList.indexOf(src);
		if(index == -1) return;

		String m = e.getMessage();

		if ((m != null) && (m.length() > 0)) {

			if (m.equals(LOCK_PROGRESS_MESSAGE)) {
				messageLocked = true;
			} else if (m.equals(UNLOCK_PROGRESS_MESSAGE)) {
				messageLocked = false;
			}
			else if (!messageLocked) {
				setMessage(index, m);
			}
		}

		if (value != PROGRESS_VALUE_UNCHANGED) {
			updateValue(index, value);
		}
	}

	/**
	 * Method for removing the algorithm from the frame displayed
	 * as well as removing the listener attached to it
	 * @param alg the algorithm to remove
	 */
	public void removeAlgorithm(Runnable alg){
		if(!algList.contains(alg)) return;
		int index = algList.indexOf(alg);
		AlgoContainer container = algCont.get(index);
		if(container.pBar.getParent() != null){
			container.removeFromPane();
		}
	
		if(alg instanceof AlgorithmBase){
			AlgorithmBase base = (AlgorithmBase)alg;
			base.removeProgressChangeListener(this);
		}
		algList.remove(index);
		hideList.remove(index);
		closeList.remove(index);
		algCont.remove(index);
		
		if(barPanel.getComponentCount() == 0){
			barPanel.add(emptyLabel);
			emptyDisplayed = true;
	
		}
	
		pack();
		update(this.getGraphics());
	
	}

	/**
	 * Create a link between two algorithms. Any progress in the child algorithm
	 * will cause updates in the given range in the parent algorithm
	 * @param to the parent algorighm
	 * @param from the child algorithm
	 * @param min the lower bound of where the child affects the parent
	 * @param max the upper bound of where the child affects the parent
	 */
	public void setLink(Runnable to, Runnable from, int min, int max){
		
		int index = algList.indexOf(from);
		algCont.get(index).link = new Linking(to, min,max);
	}

	/**
	 * Updates the displayed message above a given progress bar
	 * @param index which algorithm to update
	 * @param msg the message to display
	 */
	public void setMessage(int index, String msg){
		
		AlgoContainer alg = algCont.get(index);
		alg.updateMessage(msg);
	}

	/**
	 * Update the specific algorithm's progress display
	 * @param index location of algorithm in the list
	 * @param value progress value to display
	 */
	public void updateValue(int index, int value) {
		
		AlgoContainer container = algCont.get(index);
		container.updateValue(value);
	}

	private void init(){

		try {
			setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
		} catch (FileNotFoundException error) {
			Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
					">.  Check that this file is available.\n");
			System.err.println("Exception ocurred while getting <" + error.getMessage() +
					">.  Check that this file is available.\n");
		}

		if (cancelFlag) {
			JPanel buttonPanel = new JPanel();

			if (actionListener != null) {
				cancelButton.addActionListener(actionListener);
			} else {
				cancelButton.addActionListener(this);
			}

			cancelButton.setToolTipText("Stop process");
			cancelButton.setMnemonic('d');
			cancelButton.setFont(MipavUtil.font12B);
			cancelButton.setPreferredSize(new Dimension(90, 30));
			cancelButton.setActionCommand("cancel");
			buttonPanel.add(cancelButton);
			getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		}

		barPanel = new JPanel(new GridBagLayout());
		barPanel.setForeground(Color.black);
		barPanel.add(emptyLabel);
		emptyDisplayed = true;

		getContentPane().add(barPanel, BorderLayout.CENTER);

		if (windowListener != null) {
			addWindowListener(windowListener);
		} else {
			addWindowListener(this);

			// Note: this doesn't get triggered when called from the same Thread
			setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		}

		pack();
		setVisible(true);
		MipavUtil.centerOnScreen(this);
	}

	/**
	 * This listens for when the buttons to hide or close a progress bar
	 * are clicked. It will automatically determine which bar to choose.
	 */
	@Override
	public void mouseClicked(MouseEvent e) {
		Object src = e.getSource();
		int index;
		if(hideList.contains(src)){
			index = hideList.indexOf(src);
			AlgoContainer container = algCont.get(index);
			if(container.hideLabel.getText().equals("-")){
				container.hide();
			} else {
				container.show();
			}
		pack();
		update(this.getGraphics());
		}
		else if(closeList.contains(src)){
			index = closeList.indexOf(src);
			Runnable alg = algList.get(index);
			removeAlgorithm(alg);
		}

		
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

	/**
	 * Very basic structure to update determine which algorithm is linked, and
	 * over what range. Only one link is allowed as of now.
	 * @author wangvg
	 *
	 */
	private class Linking{

		private Runnable link;

		private int min;

		private int max;

		private Linking(Runnable alg, int min, int max){

			link = alg;
			this.min = min;
			this.max = max;

		}

	}

	/**
	 * A container class that holds all the information to correctly
	 * draw each progress bar. It also holds several methods that help
	 * in modifying the frame correctly.
	 * @author wangvg
	 *
	 */
	private class AlgoContainer{

		/**
		 * Base title string inputted at instantiation
		 */
		private String title;

		/**
		 * Actual displayed label: also includes the message
		 */
		private JLabel titleLabel;

		/**
		 * The percentage displayed next to the progress bar
		 */
		private JLabel pctLabel;

		/**
		 * Linking information for sub-algorithms
		 */
		private Linking link;

		/**
		 * The associated label to hide/show the bar
		 */
		private JLabel hideLabel;

		/**
		 * The associated label to close the bar
		 */
		private JLabel closeLabel;

		/**
		 * The progress bar
		 */
		private JProgressBar pBar;

		private AlgoContainer(String _title, int min, int max){
			
			title = _title;

			titleLabel = new JLabel(title + ": ");
			titleLabel.setFont(MipavUtil.font12B);
			titleLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			titleLabel.setForeground(Color.black);

			pBar = new JProgressBar(min, max);
			pBar.setValue(0);
			pBar.setPreferredSize(new Dimension(220, 15));
			pBar.setMinimumSize(new Dimension(50, 10));
			pBar.setAlignmentX(JComponent.CENTER_ALIGNMENT);

			pctLabel = new JLabel("0%");
			pctLabel.setFont(MipavUtil.font12B);
			pctLabel.setPreferredSize(new Dimension(30, 20));
			pctLabel.setMinimumSize(new Dimension(30, 20));

			hideLabel = new JLabel("-");
			hideLabel.setFont(new Font("Monospaced", Font.BOLD, 16));
			hideLabel.addMouseListener(ViewJProgressBarMulti.this);
			hideList.add(hideLabel);

			closeLabel = new JLabel("x");
			closeLabel.setFont(new Font("Monospaced", Font.BOLD, 16));
			closeLabel.addMouseListener(ViewJProgressBarMulti.this);
			closeList.add(closeLabel);

			link = new Linking(null, 0, 0);
			
			addToPane();
		}

		/**
		 * Adds what the container just built (based on the given inputs) to the 
		 * component barPanel. 
		 */
		private void addToPane(){
			
			if(emptyDisplayed){
				barPanel.remove(emptyLabel);
				emptyDisplayed = false;
			}

			gbc.weightx = 1;
			gbc.fill = GridBagConstraints.HORIZONTAL;
			gbc.anchor = GridBagConstraints.WEST;
			gbc.insets = new Insets(5, 5, 5, 5);
			barPanel.add(titleLabel, gbc);

			gbc.insets = new Insets(0, 1, 0, 1);
			gbc.weightx = 0;
			gbc.anchor = GridBagConstraints.EAST;
			barPanel.add(hideLabel, gbc);

			gbc.anchor = GridBagConstraints.WEST;
			barPanel.add(closeLabel, gbc);
			
			gbc.gridy++;
			gbc.insets = new Insets(5, 5, 5, 5);
			gbc.fill = GridBagConstraints.BOTH;
			gbc.weightx = 1;
			gbc.weighty = 1;
			barPanel.add(pBar, gbc);

			gbc.gridwidth = 2;
			gbc.fill = GridBagConstraints.NONE;
			gbc.weightx = 0;
			gbc.weighty = 0;
			barPanel.add(pctLabel, gbc);

			gbc.gridwidth = 1;
			gbc.gridy++;

			getContentPane().add(barPanel, BorderLayout.CENTER);
			pack();
			update(ViewJProgressBarMulti.this.getGraphics());
		}
		
		/**
		 * Removes the components from this given container from
		 * the frame
		 */
		private void removeFromPane(){
			barPanel.remove(pBar);
			barPanel.remove(pctLabel);
			barPanel.remove(titleLabel);
			barPanel.remove(hideLabel);
			barPanel.remove(closeLabel);
			
			pack();
			update(ViewJProgressBarMulti.this.getGraphics());
		}
		
		/**
		 * Hide only the progress bar and the percentage text
		 */
		private void hide(){
			
			pBar.setVisible(false);
			pctLabel.setVisible(false);
			hideLabel.setText("+");
			
			pack();
			update(ViewJProgressBarMulti.this.getGraphics());
		}
		
		/**
		 * Display the hidden progress bar and text
		 */
		private void show(){
			pBar.setVisible(true);
			pctLabel.setVisible(true);
			hideLabel.setText("-");
			
			pack();
			update(ViewJProgressBarMulti.this.getGraphics());
		}
		
		private void updateMessage(String msg){
			titleLabel.setText(title + ": " + msg);
		}
		
		/**
		 * Updates this containers components based on the input value
		 * @param value the new progress value
		 */
		private void updateValue(int value){
			if(value == PROGRESS_WINDOW_CLOSING){				
				titleLabel.setText(title + ": COMPLETED");
				pctLabel.setText("100%");
				pBar.setValue(100);

				if(link.link != null){
					int max = link.max;
					int newInd = algList.indexOf(link.link);
					AlgoContainer alg = algCont.get(newInd);
					alg.pBar.setValue(max);
					alg.pctLabel.setText(String.valueOf(max) + "%");
				}
			}
			else{
				pBar.setValue(value);
				pctLabel.setText(String.valueOf(value) + "%");
				
				if(link.link != null){
					int min = link.min;
					int max = link.max;
					int range = max - min;
					int step = range * value / 100;
					int newProgress = min + step;
					int newInd = algList.indexOf(link.link);
					AlgoContainer alg = algCont.get(newInd);
					alg.pBar.setValue(newProgress);
					alg.pctLabel.setText(String.valueOf(newProgress) + "%");
				}
			}
			/*
			 * For some reason, this bottom three lines now suddenly make the progressbar flicker
			 */
			update(ViewJProgressBarMulti.this.getGraphics());
			
		}
	}
}
