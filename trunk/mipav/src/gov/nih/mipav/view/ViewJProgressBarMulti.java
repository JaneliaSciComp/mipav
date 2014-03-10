package gov.nih.mipav.view;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ProgressChangeEvent;
import gov.nih.mipav.view.ViewJProgressBar;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
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

public class ViewJProgressBarMulti extends ViewJProgressBar implements MouseListener{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1298459723854411415L;

	private JPanel barPanel;
	
	private ArrayList<JProgressBar> pBars;
	
	private ArrayList<Runnable> algList;
	
	private ArrayList<JLabel> titleList;
	
	private ArrayList<JLabel> percents;
	
	private ArrayList<Boolean> completed;
	
	//private boolean readyToDispose = true;
	
	private boolean messageLocked = false;
	
	private boolean separateThread = true;
	
	private ActionListener actionListener;
	
	private WindowListener windowListener;
	
	private boolean cancelFlag;
	
	private JButton cancelButton;
	
	private JLabel emptyLabel = new JLabel("No threads registered");
	
	private boolean emptyDisplayed;
	
	private GridBagConstraints gbc;
	
	private int currentRow;
	
	private ArrayList<JLabel> hideList;
	
	private ArrayList<Linking> linkInfo;
	
	private ArrayList<String> titles;
	
	private ArrayList<String> msgList;
	
	public ViewJProgressBarMulti(String _title, boolean _cancelFlag,
            ActionListener _actionListener, WindowListener _windowListener) {
		
		super(_title, null, 0, 100, _cancelFlag, _actionListener, _windowListener, false);
		
		pBars = new ArrayList<JProgressBar>();
		algList = new ArrayList<Runnable>();
		titleList = new ArrayList<JLabel>();
		percents = new ArrayList<JLabel>();
		hideList = new ArrayList<JLabel>();
		completed = new ArrayList<Boolean>();
		linkInfo = new ArrayList<Linking>();
		titles = new ArrayList<String>();
		msgList = new ArrayList<String>();
		
		windowListener = _windowListener;
		actionListener = _actionListener;
		cancelFlag = _cancelFlag;
		cancelButton = getCancelButton();
		
		gbc = new GridBagConstraints();
		currentRow = 0;
		
		
		emptyLabel.setPreferredSize(new Dimension(270,45));
		
		getContentPane().removeAll();
		init();

		
	}
	
	public void addRunnable(Runnable alg, String title, int min, int max){
		
		algList.add(alg);
		titles.add(title);

		JLabel titleLabel = new JLabel(title + ": ");
		titleLabel.setFont(MipavUtil.font12B);
		titleLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		titleLabel.setForeground(Color.black);
		titleList.add(titleLabel);

		JProgressBar pBar = new JProgressBar(min, max);
		pBar.setValue(0);
		pBar.setPreferredSize(new Dimension(220, 15));
		pBar.setMinimumSize(new Dimension(50, 10));
		pBar.setAlignmentX(JComponent.CENTER_ALIGNMENT);
		pBars.add(pBar);

		JLabel pctLabel = new JLabel("0%");
		pctLabel.setFont(MipavUtil.font12B);
		pctLabel.setPreferredSize(new Dimension(30, 20));
		pctLabel.setMinimumSize(new Dimension(30, 20));
		percents.add(pctLabel);
		completed.add(false);
		
		JLabel hideLabel = new JLabel("hide");
		hideLabel.setFont(MipavUtil.font12B);
		hideLabel.addMouseListener(this);
		hideList.add(hideLabel);
		
		linkInfo.add(new Linking(null, 0, 0));
		
		msgList.add("");

		if(emptyDisplayed){
			 barPanel.remove(emptyLabel);
			 emptyDisplayed = false;
		}

		gbc.gridy = currentRow;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		barPanel.add(titleLabel, gbc);
		
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		barPanel.add(hideLabel, gbc);

		gbc.anchor = GridBagConstraints.WEST;
		gbc.gridy = currentRow + 1;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		barPanel.add(pBar, gbc);

		gbc.fill = GridBagConstraints.NONE;
		gbc.weightx = 0;
		gbc.weighty = 0;
		barPanel.add(pctLabel, gbc);

		currentRow +=2;

		getContentPane().add(barPanel, BorderLayout.CENTER);
		pack();
		update(this.getGraphics());
		
		
		
	}
	
	public void addCollectionOfRunnable(Collection<Runnable> algs, Collection<String> titles){
		if(algs.size() !=  titles.size()){
			System.err.println("Number of messages does not match with number of threads");
			return;
		}
		algList.addAll(algs);
		JProgressBar pBar = null;
		JLabel titleLabel = null;
		JLabel pctLabel = null;
		Iterator<String> iter = titles.iterator();
		JLabel hideLabel = null;
		
		
		if(emptyDisplayed){
			 barPanel.remove(emptyLabel);
			 emptyDisplayed = false;
		 }
		
		String title;
		
		for(int i=0;i<algs.size();i++){
			pBar = new JProgressBar(0, 100);
			pBar.setValue(0);
	        pBar.setPreferredSize(new Dimension(220, 15));
	        pBar.setMinimumSize(new Dimension(50, 10));
	        pBar.setAlignmentX(JComponent.CENTER_ALIGNMENT);
			pBars.add(pBar);
			
			title = iter.next();
			titles.add(title);
			
			titleLabel = new JLabel(title + ": ");
			titleLabel.setFont(MipavUtil.font12B);
	        titleLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
	        titleLabel.setForeground(Color.black);
			titleList.add(titleLabel);
			
			pctLabel = new JLabel("0%");
			pctLabel.setFont(MipavUtil.font12B);
	        pctLabel.setPreferredSize(new Dimension(30, 20));
	        pctLabel.setMinimumSize(new Dimension(30, 20));
			percents.add(pctLabel);
			completed.add(false);
			
			hideLabel = new JLabel("hide");
			hideLabel.setFont(MipavUtil.font12B);
			hideLabel.addMouseListener(this);
			hideList.add(hideLabel);
			
			linkInfo.add(new Linking(null, 0, 0));
			
			msgList.add("");
			
			gbc.gridy = 2*i + currentRow;
			gbc.weightx = 1;
			gbc.fill = GridBagConstraints.HORIZONTAL;
			gbc.anchor = GridBagConstraints.WEST;
			gbc.insets = new Insets(5, 5, 5, 5);
			barPanel.add(titleLabel, gbc);
			
			gbc.weightx = 0;
			gbc.anchor = GridBagConstraints.EAST;
			barPanel.add(hideLabel, gbc);

			gbc.anchor = GridBagConstraints.WEST;
			gbc.gridy = 2*i + 1 + currentRow;
			gbc.fill = GridBagConstraints.BOTH;
			gbc.weightx = 1;
			gbc.weighty = 1;
			barPanel.add(pBar, gbc);

			gbc.fill = GridBagConstraints.NONE;
			gbc.weightx = 0;
			gbc.weighty = 0;
			barPanel.add(pctLabel, gbc);
		}

		 currentRow += 2*algs.size();
		 
		 getContentPane().add(barPanel, BorderLayout.CENTER);
		 pack();
		 update(this.getGraphics());

	}
	
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

        /*if (!completed.contains(false) && readyToDispose) {
            setVisible(false);

            return;
        }*/
        
        
        /*if ((!completed.contains(false)) && (value == PROGRESS_WINDOW_CLOSING || readyToDispose)) {
            setVisible(false);

            return;
        }*/
        
        

       /* String t = e.getTitle();

        if ((t != null) && (t.length() > 0)) {
            setTitle(t);
        }*/

        /*String m = e.getMessage();

                
        if ((m != null) && (m.length() > 0)) {
        	
        	if (m.equals(LOCK_PROGRESS_MESSAGE)) {
        		messageLocked = true;
        	} else if (m.equals(UNLOCK_PROGRESS_MESSAGE)) {
        		messageLocked = false;
        	}
        	else if (!messageLocked) {
        		setMessage(m);
        	}
        }*/

        /** Put this in here so you can change the message without updating the value */
        /*if (value != PROGRESS_VALUE_UNCHANGED) {
            updateValue(value);
        }*/
    }
	
	 public void updateValue(int index, int value) {
		 
		 if(value == PROGRESS_WINDOW_CLOSING){
			 /*barPanel.remove(pBars.get(index));
			 barPanel.remove(percents.get(index));*/
			 setMessage(index, "COMPLETED");
			 percents.get(index).setText("100%");
			 pBars.get(index).setValue(100);
			 completed.set(index, true);
			 
			 Linking info = linkInfo.get(index);
			 if(info.link != null){
				 int max = info.max;
				 int newInd = algList.indexOf(info.link);
				 pBars.get(newInd).setValue(max);
				 percents.get(newInd).setText(String.valueOf(max) + "%");
			 }
			 
			 //pack();
			 update(this.getGraphics());
		 }
		 else{
			 pBars.get(index).setValue(value);
			 percents.get(index).setText(String.valueOf(value) + "%");
			 Linking info = linkInfo.get(index);
			 if(info.link != null){
				 int min = info.min;
				 int max = info.max;
				 int range = max - min;
				 int step = range * value / 100;
				 int newProgress = min + step;
				 int newInd = algList.indexOf(info.link);
				 pBars.get(newInd).setValue(newProgress);
				 percents.get(newInd).setText(String.valueOf(newProgress) + "%");
			 }
		 }
		 

		 /*
		  * For some reason, this bottom three lines now suddenly make the progressbar flicker
		  */
		 if (!this.separateThread) {
			 update(this.getGraphics());
		 }
	 }
	 
	 public void setLink(Runnable to, Runnable from, int min, int max){
		 int index = algList.indexOf(from);
		 linkInfo.set(index, new Linking(to, min,max));
	 }
	 
	 public void setMessage(int index, String msg){
		 msgList.set(index, msg);
		 titleList.get(index).setText(titles.get(index) + ": " + msg);
		 
	 }
	 
	 /*private void buildBarPanel(){

		 //barPanel = new JPanel();
		 if(emptyDisplayed){
			 barPanel.remove(emptyLabel);
			 emptyDisplayed = false;
		 }
		 
		 //barPanel.removeAll();
		 
		 for(int i=0;i<algList.size();i++){
			 gbc.gridy = 2*i + currentRow;
			 gbc.weightx = 1;
			 gbc.fill = GridBagConstraints.HORIZONTAL;
			 gbc.anchor = GridBagConstraints.WEST;
			 gbc.insets = new Insets(5, 5, 5, 5);
			 barPanel.add(titleList.get(i), gbc);

			 gbc.gridy = 2*i + 1 + currentRow;
			 gbc.fill = GridBagConstraints.BOTH;
			 gbc.weightx = 1;
			 gbc.weighty = 1;
			 barPanel.add(pBars.get(i), gbc);
			 
			 gbc.fill = GridBagConstraints.NONE;
			 gbc.weightx = 0;
			 gbc.weighty = 0;
			 barPanel.add(percents.get(i), gbc);
		 }
		 
		 currentRow = 2*algList.size();
		 
		 getContentPane().add(barPanel, BorderLayout.CENTER);
		 pack();
		 update(this.getGraphics());
		 

	 }*/
	 
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
		 
		 	//buildBarPanel();
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

	@Override
	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		Object src = e.getSource();
		int index = hideList.indexOf(src);

		if(hideList.get(index).getText().equals("<html><u>hide</u><html>")){
			//barPanel.remove(pBars.get(index));
			//barPanel.remove(titleList.get(index));
			//barPanel.remove(percents.get(index));
			//barPanel.remove(hideList.get(index));
			pBars.get(index).setVisible(false);
			percents.get(index).setVisible(false);
			hideList.get(index).setText("show");
			//hideList.get(index).removeMouseListener(this);
		} else {
			/*GridBagConstraints old = (GridBagConstraints) titleList.get(index).getLayout();
			old.anchor = GridBagConstraints.WEST;
			old.gridy += 1;
			old.fill = GridBagConstraints.BOTH;
			old.weightx = 1;
			old.weighty = 1;
			barPanel.add(pBars.get(index), old);

			old.fill = GridBagConstraints.NONE;
			old.weightx = 0;
			old.weighty = 0;
			barPanel.add(percents.get(index), old);*/
			pBars.get(index).setVisible(true);
			percents.get(index).setVisible(true);
			hideList.get(index).setText("hide");
		}
		if(barPanel.getComponentCount() == 0){
			barPanel.add(emptyLabel);
			emptyDisplayed = true;
		}
		
		pack();
		update(this.getGraphics());
		
	}

	@Override
	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		Object src = e.getSource();
		int index = hideList.indexOf(src);
		String text = hideList.get(index).getText();
		JLabel srcLabel = hideList.get(index);
		srcLabel.setText("<html><u>" + text + "</u><html>");
		
	}

	@Override
	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		Object src = e.getSource();
		int index = hideList.indexOf(src);
		String text = hideList.get(index).getText();
		JLabel srcLabel = hideList.get(index);
		srcLabel.setText("<html><u>" + text + "</u><html>");
	}
	
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
	

	

}
