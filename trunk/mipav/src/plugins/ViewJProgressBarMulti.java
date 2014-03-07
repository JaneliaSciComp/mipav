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
import java.awt.event.WindowListener;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.*;

public class ViewJProgressBarMulti extends ViewJProgressBar {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1298459723854411415L;

	private JPanel barPanel;
	
	private ArrayList<JProgressBar> pBars;
	
	private ArrayList<Runnable> algList;
	
	private ArrayList<JLabel> msgList;
	
	private ArrayList<JLabel> percents;
	
	private ArrayList<Boolean> completed;
	
	private boolean readyToDispose = true;
	
	private boolean messageLocked = false;
	
	private boolean separateThread = true;
	
	private ActionListener actionListener;
	
	private WindowListener windowListener;
	
	private boolean cancelFlag;
	
	private JButton cancelButton;
	
	public ViewJProgressBarMulti(String _title, int min, int max, boolean _cancelFlag,
            ActionListener _actionListener, WindowListener _windowListener) {
		
		super(_title, null, min, max, _cancelFlag, _actionListener, _windowListener, false);
		
		pBars = new ArrayList<JProgressBar>();
		algList = new ArrayList<Runnable>();
		msgList = new ArrayList<JLabel>();
		percents = new ArrayList<JLabel>();
		completed = new ArrayList<Boolean>();
		windowListener = _windowListener;
		actionListener = _actionListener;
		cancelFlag = _cancelFlag;
		cancelButton = getCancelButton();
		
		getContentPane().removeAll();
		
		//Initialize dialog here
		
	}
	
	public ViewJProgressBarMulti(String _title, int min, int max, boolean cancelFlag,
            ActionListener actionListener, WindowListener windowListener, 
            Collection<Runnable> algs, Collection<String> msgs) {
		
		super(_title, null, min, max, cancelFlag, actionListener, windowListener, false);
		
		pBars = new ArrayList<JProgressBar>();
		algList = new ArrayList<Runnable>();
		msgList = new ArrayList<JLabel>();
		percents = new ArrayList<JLabel>();
		completed = new ArrayList<Boolean>();
		
		addCollectionOfRunnable(algs, msgs);
		
		getContentPane().removeAll();
		init();

	}
	
	public void addRunnable(Runnable alg, String msg, int min, int max){
		algList.add(alg);
		
		JLabel msgLabel = new JLabel(msg);
		msgLabel.setFont(MipavUtil.font12B);
        msgLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        msgLabel.setForeground(Color.black);
		msgList.add(msgLabel);
		
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
	}
	
	public void addCollectionOfRunnable(Collection<Runnable> algs, Collection<String> msgs){
		if(algs.size() !=  msgs.size()){
			System.err.println("Number of messages does not match with number of threads");
			return;
		}
		algList.addAll(algs);
		//msgList.addAll(msgs);
		JProgressBar pBar = null;
		JLabel msgLabel = null;
		JLabel pctLabel = null;
		Iterator<String> iter = msgs.iterator();

		for(int i=0;i<algs.size();i++){
			pBar = new JProgressBar(0, 100);
			pBar.setValue(0);
	        pBar.setPreferredSize(new Dimension(220, 15));
	        pBar.setMinimumSize(new Dimension(50, 10));
	        pBar.setAlignmentX(JComponent.CENTER_ALIGNMENT);
			pBars.add(pBar);
			
			msgLabel = new JLabel(iter.next());
			msgLabel.setFont(MipavUtil.font12B);
	        msgLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
	        msgLabel.setForeground(Color.black);
			msgList.add(msgLabel);
			
			pctLabel = new JLabel("0%");
			pctLabel.setFont(MipavUtil.font12B);
	        pctLabel.setPreferredSize(new Dimension(30, 20));
	        pctLabel.setMinimumSize(new Dimension(30, 20));
			percents.add(pctLabel);
			completed.add(false);
		}

	}
	
	public void progressStateChanged(ProgressChangeEvent e) {
		
        int value = e.getValue();
        Object src = e.getSource();
        
        int index = algList.indexOf(src);
        
        String m = e.getMessage();
        
        if ((m != null) && (m.length() > 0)) {
        	
        	if (m.equals(LOCK_PROGRESS_MESSAGE)) {
        		messageLocked = true;
        	} else if (m.equals(UNLOCK_PROGRESS_MESSAGE)) {
        		messageLocked = false;
        	}
        	else if (!messageLocked) {
        		setMessage(m);
        	}
        }
        
        if (value != PROGRESS_VALUE_UNCHANGED) {
            updateValue(index, value);
        }

        if ((value == PROGRESS_WINDOW_CLOSING) || (!completed.contains(false) && readyToDispose)) {
            setVisible(false);

            return;
        }
        
        

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
		 
		 pBars.get(index).setValue(value);
		 percents.get(index).setText(String.valueOf(value) + "%");
		 
		 if(value == 100){
			 completed.set(index, true);
		 }

		 /*
		  * For some reason, this bottom three lines now suddenly make the progressbar flicker
		  */
		 if (!this.separateThread) {
			 update(this.getGraphics());
		 }
	 }
	 
	 private void buildBarPanel(){

		 barPanel = new JPanel();
		 barPanel.setLayout(new GridBagLayout());
		 GridBagConstraints gbc = new GridBagConstraints();
		 
		 barPanel.removeAll();
		 
		 for(int i=0;i<algList.size();i++){
			 gbc.gridy = 2*i;
			 gbc.weightx = 1;
			 gbc.fill = GridBagConstraints.HORIZONTAL;
			 gbc.anchor = GridBagConstraints.WEST;
			 gbc.insets = new Insets(5, 5, 5, 5);
			 barPanel.add(msgList.get(i), gbc);

			 gbc.gridy = 2*i + 1;
			 gbc.fill = GridBagConstraints.BOTH;
			 gbc.weightx = 1;
			 gbc.weighty = 1;
			 barPanel.add(pBars.get(i), gbc);
			 
			 gbc.fill = GridBagConstraints.NONE;
			 gbc.weightx = 0;
			 gbc.weighty = 0;
			 barPanel.add(percents.get(i), gbc);
		 }

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
		 
		 	buildBarPanel();
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
	

	

}
