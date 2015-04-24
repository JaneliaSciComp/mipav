package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.CustomUIBuilder.UIParams;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRenderBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase.CancelListener;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelSurface_WM.OkColorListener;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;


public class JPanelAnnotationAnimation extends JInterfaceBase implements ChangeListener, ListSelectionListener, KeyListener, MouseListener {

//    private JCheckBox[] checkboxList;
//    private JButton[] colorButtonList;
//    private JCheckBox[] labelCheckList;
//    private JButton[] labelColorList;
    private JSlider annimationSlider;
    private JLabel timeLabel;
    private VolumeTriPlanarRender parent;

    /** Scroll pane. */
    private JScrollPane scroller;
    
    public JPanelAnnotationAnimation( VolumeTriPlanarRender parent, int timeSteps, Vector<String> annotationNames )
    {
        super();
        this.parent = parent;
//        init( timeSteps, annotationNames );
        init( annotationNames, timeSteps );
    }
    
//    private void init( int timeSteps, Vector<String> annotations )
//    {
//    	String[] names = new String[annotations.size()];
//    	for ( int i = 0; i < annotations.size(); i++ )
//    	{
//    		names[i] = annotations.elementAt(i);
//    	}
//    	
//        Arrays.sort( names );
//    	int size = annotations.size();
//    	JPanel annotationPanel = new JPanel();
//    	JPanel labelsPanel = new JPanel();
//    	annotationPanel.setLayout(new GridBagLayout());
//    	annotationPanel.setForeground(Color.white);
//    	annotationPanel.setBackground(Color.white);
//    	
//    	labelsPanel.setLayout(new GridBagLayout());
//    	labelsPanel.setForeground(Color.white);
//    	labelsPanel.setBackground(Color.white);
//        
//        checkboxList = new JCheckBox[size];
//        colorButtonList = new JButton[size];
//        labelCheckList = new JCheckBox[size];
//        labelColorList = new JButton[size];
//        
//        GridBagConstraints gbc = new GridBagConstraints();
//        gbc.gridx = 0;
//        gbc.gridy = 0;
////        gbc.gridwidth = 1;
////        gbc.gridheight = 1;
//
//        for (int i = 0; i < size; i++) { // place nSlices of check options for user and give them a name
//            checkboxList[i] = new JCheckBox( names[i], true );
//            checkboxList[i].addActionListener(this);
//            checkboxList[i].setBackground(Color.white);
//            annotationPanel.add(checkboxList[i], gbc); gbc.gridx++;
//
//            colorButtonList[i] = new JButton();
//            colorButtonList[i].addActionListener(this);
//            colorButtonList[i].setMinimumSize(new Dimension(30, 30));
//            colorButtonList[i].setMaximumSize(new Dimension(30, 30));
//            colorButtonList[i].setPreferredSize(new Dimension(30, 30));
//            annotationPanel.add(colorButtonList[i], gbc); gbc.gridx++;
////            annotationPanel.add(new JLabel(" "), gbc); gbc.gridx++;
//            
//
//            gbc.gridx = 0;
//            labelCheckList[i] = new JCheckBox( "show label", true );
//            labelCheckList[i].addActionListener(this);
//            labelCheckList[i].setBackground(Color.white);
//            labelsPanel.add(labelCheckList[i], gbc); gbc.gridx++;
//            
//            labelColorList[i] = new JButton();
//            labelColorList[i].addActionListener(this);
//            labelColorList[i].setMinimumSize(new Dimension(30, 30));
//            labelColorList[i].setMaximumSize(new Dimension(30, 30));
//            labelColorList[i].setPreferredSize(new Dimension(30, 30));
//            labelsPanel.add(labelColorList[i], gbc); gbc.gridx++;
////            labelsPanel.add(new JLabel(" "), gbc); gbc.gridx++;
//            
//            gbc.gridx = 0;
//            gbc.gridy++;
//        }
//        JPanel annotationCheckPanel = new JPanel(new GridBagLayout());
//
//        // make check & uncheck buttons for the panel--place inside the above border
//        JButton checkButton = new JButton("Select all");
//        checkButton.setPreferredSize(new Dimension(95, 30));
//        checkButton.setMinimumSize(new Dimension(95, 30));
//        annotationCheckPanel.add(checkButton, gbc);
//        checkButton.addActionListener(this);
//        checkButton.setActionCommand("SelectAll");
//
//        gbc.gridx = 1;
//        JButton unCheckButton = new JButton("Clear");
//        unCheckButton.setPreferredSize(new Dimension(95, 30));
//        unCheckButton.setMinimumSize(new Dimension(95, 30));
//        unCheckButton.addActionListener(this);
//        unCheckButton.setActionCommand("ClearAll");
//        annotationCheckPanel.add(unCheckButton, gbc);
//        
//
//        JPanel labelCheckPanel = new JPanel(new GridBagLayout());
//        gbc = new GridBagConstraints();
//
//        gbc.gridx = 0;
//        gbc.gridy = 0;
//        gbc.gridwidth = 1;
//        gbc.gridheight = 1;
//
//        // make check & uncheck buttons for the panel--place inside the above border
//        checkButton = new JButton("Select all");
//        checkButton.setPreferredSize(new Dimension(95, 30));
//        checkButton.setMinimumSize(new Dimension(95, 30));
//        labelCheckPanel.add(checkButton, gbc);
//        checkButton.addActionListener(this);
//        checkButton.setActionCommand("SelectAllLabels");
//
//        gbc.gridx = 1;
//        unCheckButton = new JButton("Clear");
//        unCheckButton.setPreferredSize(new Dimension(95, 30));
//        unCheckButton.setMinimumSize(new Dimension(95, 30));
//        unCheckButton.addActionListener(this);
//        unCheckButton.setActionCommand("ClearAllLabels");
//        labelCheckPanel.add(unCheckButton, gbc);
//
//
//        JPanel annotationListPanel = new JPanel(new BorderLayout());
//        annotationListPanel.add(annotationPanel, BorderLayout.NORTH);        
//        annotationListPanel.add(annotationCheckPanel, BorderLayout.CENTER);
//
//        JPanel annotationLlabelPanel = new JPanel(new BorderLayout());
//        annotationLlabelPanel.add(labelsPanel, BorderLayout.NORTH);        
//        annotationLlabelPanel.add(labelCheckPanel, BorderLayout.CENTER);
//        
//        JPanel dualPanel = new JPanel( new GridLayout(1,2) );
//        dualPanel.add(annotationListPanel);
//        dualPanel.add(annotationLlabelPanel);        
//        
//
//        JButton playButton = new JButton("Play");
//        playButton.addActionListener(this);
//        playButton.setActionCommand("Play");
//        playButton.setFont(MipavUtil.font12B);
//        playButton.setPreferredSize(MipavUtil.defaultButtonSize);
//
//        JButton stopButton = new JButton("Stop");
//        stopButton.addActionListener(this);
//        stopButton.setActionCommand("Stop");
//        stopButton.setFont(MipavUtil.font12B);
//        stopButton.setPreferredSize(MipavUtil.defaultButtonSize);
//
//        JButton recordButton = new JButton("Record");
//        recordButton.addActionListener(this);
//        recordButton.setActionCommand("Record");
//        recordButton.setFont(MipavUtil.font12B);
//        recordButton.setPreferredSize(MipavUtil.defaultButtonSize);
//        
//
//        JLabel timeSlice = new JLabel("Time: ");
//        timeLabel = new JLabel("0");
//        annimationSlider = new JSlider(0, timeSteps-1, 0 );
//        annimationSlider.setMajorTickSpacing(1);
////        annimationSlider.setMinorTickSpacing(1);
////        annimationSlider.setPaintTicks(true);
//        annimationSlider.addChangeListener(this);
////        annimationSlider.setPaintLabels(true);
//        annimationSlider.setSnapToTicks(true);
//        annimationSlider.setEnabled(true);
//        
//        
//        GridBagConstraints kGBC = new GridBagConstraints();
//        GridBagLayout kGrid = new GridBagLayout();
//        kGBC.gridx = 0;
//        kGBC.gridy = 0;
//        
//        JPanel kAnimatePanel = new JPanel(kGrid);
//        kAnimatePanel.setBorder(buildTitledBorder("Animation"));
//        kAnimatePanel.add( playButton, kGBC ); kGBC.gridx++;
//        kAnimatePanel.add( stopButton, kGBC ); kGBC.gridx++;
//        kAnimatePanel.add( recordButton, kGBC ); kGBC.gridx++;
//        kGBC.gridx = 0;
//        kGBC.gridy = 1;
//        
//        kAnimatePanel.add( timeSlice, kGBC ); kGBC.gridx++;
//        kAnimatePanel.add( annimationSlider, kGBC ); kGBC.gridx++;
//        kAnimatePanel.add( timeLabel, kGBC ); kGBC.gridx++;
//                
//        
//        JPanel interfacePanel = new JPanel(new BorderLayout());        
//        interfacePanel.add(dualPanel, BorderLayout.NORTH);
//        interfacePanel.add(kAnimatePanel, BorderLayout.CENTER);
//
//
//
//        // make the list scroll if there are enough checkboxes
//        scroller = new JScrollPane(interfacePanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
//                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
//        
//        mainPanel = new JPanel(new BorderLayout());       
//        mainPanel.add(scroller, BorderLayout.NORTH); 
//        
//        getContentPane().add(mainPanel);
//        pack();
//    }
    
    
    private JTabbedPane neuriteTabbedPane;
    private JCheckBox displaySurface;
    private JCheckBox displayLabel;
    private JList<String> surfaceList;
    private Vector<JList<String>> neuriteList;
    private JTextField diameter;
    private Vector<JCheckBox> displayNeurite;
    private Vector<JTextField> diameterNeurite;
    private void init( Vector<String> annotations, int timeSteps ) {
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());

        JPanel surfacePanel = new JPanel();
        displaySurface = new JCheckBox("display", true);
        displaySurface.addActionListener(this);
        displaySurface.setActionCommand("display");

        JButton colorButton = new JButton("color");
        colorButton.addActionListener(this);
        colorButton.setActionCommand("color");

        diameter = new JTextField("1.0");
//        diameter.addFocusListener( this );
        diameter.addKeyListener( this );

        surfacePanel.add( new JLabel("Annotation: " ) );
        surfacePanel.add(displaySurface);
        surfacePanel.add(colorButton);
        surfacePanel.add( new JLabel("set diameter (0.1-2.0): " ) );
        surfacePanel.add(diameter);
        


        JPanel labelPanel = new JPanel();
        displayLabel = new JCheckBox("display", true);
        displayLabel.addActionListener(this);
        displayLabel.setActionCommand("displayLabel");

        JButton fontButton = new JButton("text options");
        fontButton.addActionListener(this);
        fontButton.setActionCommand("text");
        
        labelPanel.add( new JLabel("Label: " ) );
        labelPanel.add(displayLabel);
        labelPanel.add(fontButton);
        
        JPanel displayOptions = new JPanel(new BorderLayout());
        displayOptions.add( surfacePanel, BorderLayout.NORTH );
        displayOptions.add( labelPanel, BorderLayout.SOUTH );
        
        // list panel for surface filenames
        surfaceList = new JList<String>(new DefaultListModel<String>());
        surfaceList.addListSelectionListener(this);
        surfaceList.addMouseListener( this );

        JScrollPane kScrollPane = new JScrollPane(surfaceList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(displayOptions, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("Annotation list"));
        
        //////////////////////////////////////////////////////////////////

        neuriteTabbedPane = new JTabbedPane(); 
        
        
        /////////////////////////////////////////////////////////////////
        JButton playButton = new JButton("Play");
        playButton.addActionListener(this);
        playButton.setActionCommand("Play");
        playButton.setFont(MipavUtil.font12B);
        playButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton stopButton = new JButton("Stop");
        stopButton.addActionListener(this);
        stopButton.setActionCommand("Stop");
        stopButton.setFont(MipavUtil.font12B);
        stopButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton recordButton = new JButton("Record");
        recordButton.addActionListener(this);
        recordButton.setActionCommand("Record");
        recordButton.setFont(MipavUtil.font12B);
        recordButton.setPreferredSize(MipavUtil.defaultButtonSize);
        

        JLabel timeSlice = new JLabel("Time: ");
        timeLabel = new JLabel("0");
        annimationSlider = new JSlider(0, timeSteps-1, 0 );
        annimationSlider.setMajorTickSpacing(1);
//        annimationSlider.setMinorTickSpacing(1);
//        annimationSlider.setPaintTicks(true);
        annimationSlider.addChangeListener(this);
//        annimationSlider.setPaintLabels(true);
        annimationSlider.setSnapToTicks(true);
        annimationSlider.setEnabled(true);
        
        
        GridBagConstraints kGBC = new GridBagConstraints();
        GridBagLayout kGrid = new GridBagLayout();
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        
        JPanel kAnimatePanel = new JPanel(kGrid);
        kAnimatePanel.setBorder(buildTitledBorder("Animation"));
        kAnimatePanel.add( playButton, kGBC ); kGBC.gridx++;
        kAnimatePanel.add( stopButton, kGBC ); kGBC.gridx++;
        kAnimatePanel.add( recordButton, kGBC ); kGBC.gridx++;
        kGBC.gridx = 0;
        kGBC.gridy = 1;
        
        kAnimatePanel.add( timeSlice, kGBC ); kGBC.gridx++;
        kAnimatePanel.add( annimationSlider, kGBC ); kGBC.gridx++;
        kAnimatePanel.add( timeLabel, kGBC ); kGBC.gridx++;

        mainScrollPanel.add(listPanel, BorderLayout.NORTH);
        mainScrollPanel.add(neuriteTabbedPane, BorderLayout.CENTER);
        mainScrollPanel.add(kAnimatePanel, BorderLayout.SOUTH);
        mainPanel.add(scroller, BorderLayout.CENTER);
        

        String[] names = new String[annotations.size()];
    	for ( int i = 0; i < annotations.size(); i++ )
    	{
    		names[i] = annotations.elementAt(i);
    	}
    	
        Arrays.sort( names );
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
        for ( int i = 0; i < names.length; i++ )
        {
        	kList.add(i, names[i]);
        }
        
        
        
        
        
        
        getContentPane().add(mainPanel);
        pack();
    }
    
    

	public void actionPerformed(ActionEvent e)
	{
		String command = e.getActionCommand();
		if ( command.equals("Play") || command.equals("Stop") )
		{
			parent.startStopVOIAnimation();
		}
		else if ( command.equals("Record") )
		{
			parent.startRecording();
		}
		else if ( command.equals( "display" ) )
		{
			displaySelected( displaySurface.isSelected() );
		}
		else if ( command.equals( "color" ) )
		{
			colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(),
					new CancelListener());
		}
		else if ( command.equals( "displayLabel" ) )
		{
			displaySelectedLabel( displayLabel.isSelected() );
		}
		else if ( command.equals( "text" ) )
		{
			setSelectedFonts();
		}
		else if ( command.equals( "newlist" ) )
		{
			newList();
		}
		else if ( command.equals( "addlist" ) )
		{
			addList();
			updateNeurites();
		}
		else if ( command.equals( "moveUp" ) )
		{
			int index = neuriteTabbedPane.getSelectedIndex();
			if ( index == -1 )
			{
				return;
			}
			JList<String> list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        if ( selected.length == 1 )
	        {
	        	if ( selected[0] > 0 )
	        	{
	        		DefaultListModel<String> kList = (DefaultListModel)list.getModel();
	        		String name = kList.remove( selected[0] );
	        		kList.add( selected[0] - 1, name );
	        		list.setSelectedIndex( selected[0] - 1 );
	        		//		        System.err.println( "moveUp " + name );
	        	}
	        }
			updateNeurites();
		}
		else if ( command.equals( "moveDown" ) )
		{
			int index = neuriteTabbedPane.getSelectedIndex();
			if ( index == -1 )
			{
				return;
			}
			JList<String> list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();	        if ( selected.length == 1 )
	        {
        		DefaultListModel<String> kList = (DefaultListModel)list.getModel();
	        	if ( (selected[0] + 1) < kList.size() )
	        	{
	        		String name = kList.remove( selected[0] );
	        		kList.add( selected[0] + 1, name );
	        		list.setSelectedIndex( selected[0] + 1 );
	        		//		        System.err.println( "moveDown " + name );
	        	}
	        }
			updateNeurites();
		}
		else if ( command.equals( "moveFirst" ) )
		{
			int index = neuriteTabbedPane.getSelectedIndex();
			if ( index == -1 )
			{
				return;
			}
			JList<String> list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        if ( selected.length == 1 )
	        {
	        	DefaultListModel<String> kList = (DefaultListModel)list.getModel();
	        	String name = kList.remove( selected[0] );
	        	kList.add( 0, name );
	        	list.setSelectedIndex( 0 );
//		        System.err.println( "moveFirst " + name );
	        }
			updateNeurites();
		}
		else if ( command.equals( "moveLast" ) )
		{
			int index = neuriteTabbedPane.getSelectedIndex();
			if ( index == -1 )
			{
				return;
			}
			JList<String> list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        if ( selected.length == 1 )
	        {
	        	DefaultListModel<String> kList = (DefaultListModel)list.getModel();
	        	String name = kList.remove( selected[0] );
	        	index = kList.size();
	        	kList.add( index, name );
	        	list.setSelectedIndex( index );
//		        System.err.println( "moveLast " + name );
	        }			
			updateNeurites();
		}
		else if ( command.equals( "delete" ) )
		{
			int index = neuriteTabbedPane.getSelectedIndex();
			if ( index == -1 )
			{
				return;
			}
			JList<String> list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        DefaultListModel<String> kList = (DefaultListModel)list.getModel();
	        for ( int i = selected.length - 1; i >= 0; i-- )
	        {
	        	String name = kList.remove( selected[i] );
//		        System.err.println( "delete " + name );
	        }
			updateNeurites();
		}
		else if ( command.contains( "displayNeurite" ) )
		{
			updateNeurites();
		}
//		else if ( command.equals("SelectAll") )
//		{    
//			for (int i = 0; i < checkboxList.length; i++)
//			{
//				checkboxList[i].removeActionListener(this);
//				checkboxList[i].setSelected(true);
//				parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
//				checkboxList[i].addActionListener(this);
//			}
//		}
//		else if ( command.equals("ClearAll") )
//		{
//			for (int i = 0; i < checkboxList.length; i++)
//			{
//				checkboxList[i].removeActionListener(this);
//				checkboxList[i].setSelected(false);
//				parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
//				checkboxList[i].addActionListener(this);
//			}
//		}
//		else if ( command.equals("SelectAllLabels") )
//		{    
//			for (int i = 0; i < labelCheckList.length; i++)
//			{
//				labelCheckList[i].removeActionListener(this);
//				labelCheckList[i].setSelected(true);
//				parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
//				labelCheckList[i].addActionListener(this);
//			}
//		}
//		else if ( command.equals("ClearAllLabels") )
//		{
//			for (int i = 0; i < labelCheckList.length; i++)
//			{
//				labelCheckList[i].removeActionListener(this);
//				labelCheckList[i].setSelected(false);
//				parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
//				labelCheckList[i].addActionListener(this);
//			}
//		}
//		for ( int i = 0; i < checkboxList.length; i++ )
//		{
//			if ( e.getSource() == checkboxList[i] )
//			{
//				parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
//				break;
//			}
//		}
//		for ( int i = 0; i < labelCheckList.length; i++ )
//		{
//			if ( e.getSource() == labelCheckList[i] )
//			{
//				parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
//				break;
//			}
//		}
//		for ( int i = 0; i < colorButtonList.length; i++ )
//		{
//			if ( e.getSource() == colorButtonList[i] )
//			{
//				colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(colorButtonList[i]),
//						new CancelListener());
//				break;
//			}
//		}
//		for ( int i = 0; i < labelColorList.length; i++ )
//		{
//			if ( e.getSource() == labelColorList[i] )
//			{
//				colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(labelColorList[i]),
//						new CancelListener());
//				break;
//			}
//		}
	}

	private void displaySelected( boolean display )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			parent.setDisplayAnnotation( kList.elementAt( selected[i] ), display );
		}
	}
	
	private void setDiameter( float value )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			parent.setAnnotationDiameter( kList.elementAt( selected[i] ), value );
		}
	}
	
	private void newList( )
	{		
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
        String[] names = new String[selected.length];
		for ( int i = 0; i < selected.length; i++ )
		{
			names[i] = kList.elementAt( selected[i] );
		}

        int index = neuriteTabbedPane.getTabCount();
        neuriteTabbedPane.addTab("Neurite Path" + (index + 1), makeNeuriteList(index, names));		
	}
	
	private void addList( )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
		int index = neuriteTabbedPane.getSelectedIndex();
        DefaultListModel<String> listModel = (DefaultListModel)neuriteList.elementAt(index).getModel();
        int modelIndex = listModel.size();
        for ( int i = 0; i < selected.length; i++ )
        {
        	listModel.add(modelIndex++, kList.elementAt( selected[i] ));
        }
	}

	private void updateNeurites()
	{
		int index = neuriteTabbedPane.getSelectedIndex();
		if ( index == -1 )
		{
			return;
		}
		JList<String> list = neuriteList.elementAt(index);
        DefaultListModel<String> kList = (DefaultListModel)list.getModel();
        String[] names = new String[ kList.size() ];
        for ( int i = 0; i < kList.size(); i++ )
        {
        	names[i] = kList.elementAt( i );
//        	System.err.println( names[i] );
        }

		String neuriteName = neuriteTabbedPane.getTitleAt(index);
		parent.addNeurite( neuriteName, names );

		parent.displayNeurite( neuriteName, displayNeurite.elementAt(index).isSelected() );
	}
	
	private JPanel makeNeuriteList( int index, String[] names )
	{
		if ( displayNeurite == null )
		{
			displayNeurite = new Vector<JCheckBox>();
			diameterNeurite = new Vector<JTextField>();
			neuriteList = new Vector<JList<String>>();
		}
		
		JPanel surfacePanel = new JPanel();
		JCheckBox neuriteCheck = new JCheckBox("displayNeurite", false);
//		neuriteCheck.setEnabled(false);
		neuriteCheck.addActionListener(this);
		neuriteCheck.setActionCommand("displayNeurite" + index);
        displayNeurite.add( neuriteCheck );

        JButton colorButton = new JButton("color");
        colorButton.setEnabled(false);
        colorButton.addActionListener(this);
        colorButton.setActionCommand("colorNeurite" + index);

        JTextField neuriteText = new JTextField("1.0");
        neuriteText.setEnabled(false);
        neuriteText.addKeyListener( this );
        diameterNeurite.add(neuriteText);

        surfacePanel.add( new JLabel("Neurite: " ) );
        surfacePanel.add(displayNeurite.elementAt(index));
        surfacePanel.add(colorButton);
        surfacePanel.add( new JLabel("set diameter (0.1-2.0): " ) );
        surfacePanel.add(diameterNeurite.elementAt(index));
        

        JPanel displayOptions = new JPanel(new BorderLayout());
        displayOptions.add( surfacePanel, BorderLayout.NORTH );

        // list panel for surface filenames
        JList<String> list = new JList<String>(new DefaultListModel<String>());
        list.addListSelectionListener(this);
        list.addMouseListener( this );
        DefaultListModel<String> listModel = (DefaultListModel)list.getModel();
        for ( int i = 0; i < names.length; i++ )
        {
        	listModel.add(i, names[i]);
        }
        neuriteList.add(list);
        
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        JPanel listOptions = new JPanel(new GridBagLayout());
        JButton up = new JButton("move up");
        up.setPreferredSize(new Dimension(100, 30));
        up.addActionListener(this);
        up.setActionCommand( "moveUp" );
        listOptions.add(up, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        JButton down = new JButton("move down");
        down.setPreferredSize(new Dimension(100, 30));
        down.addActionListener(this);
        down.setActionCommand( "moveDown" );
        listOptions.add(down, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        JButton first = new JButton("move first");
        first.setPreferredSize(new Dimension(100, 30));
        first.addActionListener(this);
        first.setActionCommand( "moveFirst" );
        listOptions.add(first, gbc);
        gbc.gridx = 0;
        gbc.gridy++;;
        JButton last = new JButton("move last");
        last.setPreferredSize(new Dimension(100, 30));
        last.addActionListener(this);
        last.setActionCommand( "moveLast" );
        listOptions.add(last, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        JButton delete = new JButton("delete");
        delete.setPreferredSize(new Dimension(100, 30));
        delete.addActionListener(this);
        delete.setActionCommand( "delete" );
        listOptions.add(delete, gbc);

        JPanel dualPanel = new JPanel( new GridLayout(1,2) );
        dualPanel.add(neuriteList.elementAt(index));
        dualPanel.add(listOptions);    

        JScrollPane kScrollPane = new JScrollPane(dualPanel);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(displayOptions, BorderLayout.SOUTH);
        
        return listPanel;
	}

	private void displaySelectedLabel( boolean display )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			parent.setDisplayAnnotationLabel( kList.elementAt( selected[i] ), display );
		}
	}
	
	private void setSelectedFonts()
	{
        int[] selected = surfaceList.getSelectedIndices();
        if ( selected.length <= 0 )
        {
        	return;
        }
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
		VOI text = parent.getSelectedVOI( kList.elementAt( selected[0] ) );
		if ( text == null )
		{
			return;
		}
		new JDialogVolumeAnnotation( parent.getImage(), text, 0, true, false, false, this);
	}
	
	public void updateFonts( VOIText inputText )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			VOI textVOI = parent.getSelectedVOI( kList.elementAt( selected[i] ) );
			VOIText text = (VOIText) textVOI.getCurves().elementAt(0);

			if ( text != inputText )
			{
				text.setFontSize( inputText.getFontSize() );
				text.setFontDescriptors( inputText.getFontDescriptors() );
				text.setFontName( inputText.getFontName() );
				text.setColor( inputText.getColor() );
				text.setBackgroundColor( inputText.getBackgroundColor() );
				text.setUseMarker( inputText.useMarker() );
				text.updateText();
			}
		}
	}
	
    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   width
     * @param  frameHeight  height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton, java.awt.Color)
     */
    public void setButtonColor(JButton _button, Color _color) {

        super.setButtonColor(_button, _color);

        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel<String> kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			parent.setAnnotationVOIColor( kList.elementAt( selected[i] ),
					new ColorRGB( _color.getRed()/255.0f, 
						      _color.getGreen()/255.0f,
						      _color.getBlue()/255.0f ) );
		}
//		
//        if ( parent != null )
//        {
//			for (int i = 0; i < colorButtonList.length; i++)
//			{
//				if ( colorButtonList[i] == _button )
//				{
//					parent.setAnnotationVOIColor( checkboxList[i].getText(),
//							new ColorRGB( _color.getRed()/255.0f, 
//									      _color.getGreen()/255.0f,
//									      _color.getBlue()/255.0f ) );
//				}
//				if ( labelColorList[i] == _button )
//				{
//					parent.setAnnotationLabelColor( checkboxList[i].getText(),
//							new ColorRGBA( _color.getRed()/255.0f, 
//									      _color.getGreen()/255.0f,
//									      _color.getBlue()/255.0f, 1 ) );
//				}
//            }
//        }
    }
    
    /**
     * Pick up the selected color and call method to change the color.
     */
    class OkColorListener implements ActionListener {

        /** Color Button */
        JButton button;
        /**
         * Creates a new OkColorListener object.
         *
         * @param  _button  DOCUMENT ME!
         */
        OkColorListener(JButton _button) {
            super();
            button = _button;
        }

        OkColorListener( ) {
            super();
        }

        /**
         * Get color from chooser and set button and color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();
            setButtonColor(button, color);
        }
    }

	@Override
	public void stateChanged(ChangeEvent e) {
        if (e.getSource() == annimationSlider)
        {
            parent.annotationVOIsUpdate( annimationSlider.getValue() );
            timeLabel.setText( String.valueOf(annimationSlider.getValue()));
        }
	}
	
	public void setAnnimationSlider( int value )
	{
        annimationSlider.removeChangeListener(this);
        annimationSlider.setValue(value);
        timeLabel.setText( String.valueOf(annimationSlider.getValue()));
        annimationSlider.addChangeListener(this);
	}

	@Override
	public void valueChanged(ListSelectionEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void keyTyped(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void keyPressed(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void keyReleased(KeyEvent e) {
		if ( e.getKeyCode() == KeyEvent.VK_ENTER )
		{
//			System.err.println( diameter.getText() );
	        if ( !JDialogBase.testParameter(diameter.getText(), 0.1, 2.0))
	        {
	        	diameter.requestFocus();
	        	diameter.selectAll();
	        }
	        else
	        {
	        	setDiameter( Float.valueOf( diameter.getText() ) );
	        }
		}		
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		if ( e.isPopupTrigger() )
		{
			createPopup(e);
		}
	}

	@Override
	public void mousePressed(MouseEvent e) {
		if ( e.isPopupTrigger() )
		{
			createPopup(e);
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		if ( e.isPopupTrigger() )
		{
			createPopup(e);
		}
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	private void createPopup(MouseEvent e)
	{
		if ( e.getSource() == surfaceList )
		{
			JPopupMenu popup = new JPopupMenu();
			popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("New List", "newlist", UIParams.INVALID_MNEMONIC, null, null), this, false) );
			popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Add to List", "addlist", UIParams.INVALID_MNEMONIC, null, null), this, false) );
			popup.show(surfaceList, e.getX(), e.getY());
		}
//		else
//		{
//			int index = neuriteTabbedPane.getSelectedIndex();
//			if ( index == -1 )
//			{
//				return;
//			}
//			JList<String> list = neuriteList.elementAt(index);
//			if ( e.getSource() == list )
//			{
//				JPopupMenu popup = new JPopupMenu();
//				popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move Up", "moveUp", UIParams.INVALID_MNEMONIC, null, null), this, false) );
//				popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move Down", "moveDown", UIParams.INVALID_MNEMONIC, null, null), this, false) );
//				popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move First", "moveFirst", UIParams.INVALID_MNEMONIC, null, null), this, false) );
//				popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move Last", "moveLast", UIParams.INVALID_MNEMONIC, null, null), this, false) );
//				popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Delete", "delete", UIParams.INVALID_MNEMONIC, null, null), this, false) );
//				popup.show(list, e.getX(), e.getY());				
//			}
//		}
	}

}
