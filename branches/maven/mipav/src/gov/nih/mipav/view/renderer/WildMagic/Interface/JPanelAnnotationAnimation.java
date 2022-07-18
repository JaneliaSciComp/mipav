package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.view.CustomUIBuilder.UIParams;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.BorderFactory;
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
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;


public class JPanelAnnotationAnimation extends JInterfaceBase implements ChangeListener, ListSelectionListener, KeyListener, MouseListener {

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
    
    private JSlider annimationSlider;
    private JLabel timeLabel;

    private VolumeTriPlanarRender parent;
    
    /** Scroll pane. */
    private JScrollPane scroller;
    
    private JTabbedPane neuriteTabbedPane;
    private JCheckBox displaySurface;
    private JCheckBox displayLabel;
    private JButton fontButton;
    private JList surfaceList;
    private Vector<JList> neuriteList;
    private JTextField sphereDiameter;
    private Vector<JCheckBox> displayNeurite;
    private Vector<JTextField> diameterNeurite;
    private Vector<JButton> neuriteColorButton;
    private String colorTrigger;
    private JButton sphereColorButton;
    private Color defaultButtonColor;
    
    public JPanelAnnotationAnimation( VolumeTriPlanarRender parent, int timeSteps, Vector<String> annotationNames )
    {
        super();
        this.parent = parent;
        init( annotationNames, timeSteps );
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
			colorTrigger = "sphere";
			colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(sphereColorButton),
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
			JList list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        if ( selected.length == 1 )
	        {
	        	if ( selected[0] > 0 )
	        	{
	        		DefaultListModel kList = (DefaultListModel)list.getModel();
	        		String name = (String)kList.remove( selected[0] );
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
			JList list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();	        if ( selected.length == 1 )
	        {
        		DefaultListModel kList = (DefaultListModel)list.getModel();
	        	if ( (selected[0] + 1) < kList.size() )
	        	{
	        		String name = (String)kList.remove( selected[0] );
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
			JList list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        if ( selected.length == 1 )
	        {
	        	DefaultListModel kList = (DefaultListModel)list.getModel();
	        	String name = (String)kList.remove( selected[0] );
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
			JList list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        if ( selected.length == 1 )
	        {
	        	DefaultListModel kList = (DefaultListModel)list.getModel();
	        	String name = (String)kList.remove( selected[0] );
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
			JList list = neuriteList.elementAt(index);
	        int[] selected = list.getSelectedIndices();
	        DefaultListModel kList = (DefaultListModel)list.getModel();
	        for ( int i = selected.length - 1; i >= 0; i-- )
	        {
	        	String name = (String)kList.remove( selected[i] );
//		        System.err.println( "delete " + name );
	        }
			updateNeurites();
			if ( kList.size() == 0 )
			{
				String neuriteName = neuriteTabbedPane.getTitleAt(index);
				MipavUtil.displayInfo( "Deleting List " + neuriteName );
				neuriteTabbedPane.remove(index);
				displayNeurite.remove(index);
				diameterNeurite.remove(index);
				neuriteList.remove(index);
				neuriteColorButton.remove(index);
			}
		}
		else if ( command.equals( "colorNeurite" ) )
		{
			int index = neuriteTabbedPane.getSelectedIndex();
			if ( index == -1 )
			{
				return;
			}

			colorTrigger = "neurite";
			colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(neuriteColorButton.elementAt(index)),
					new CancelListener());
		}
		else if ( command.contains( "displayNeurite" ) )
		{
			updateNeurites();
		}
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
	        if ( !JDialogBase.testParameter(sphereDiameter.getText(), 0.1, 2.0))
	        {
	        	sphereDiameter.requestFocus();
	        	sphereDiameter.selectAll();
	        }
	        else
	        {
	        	setDiameter( Float.valueOf( sphereDiameter.getText() ) );
	        }
		}		
	}
	
	@Override
	public void keyTyped(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void mouseClicked(MouseEvent e) {
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
	
    public void setAnnimationSlider( int value )
	{
        annimationSlider.removeChangeListener(this);
        annimationSlider.setValue(value);
        timeLabel.setText( String.valueOf(annimationSlider.getValue()));
        annimationSlider.addChangeListener(this);
	}
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton, java.awt.Color)
     */
    public void setButtonColor(JButton _button, Color _color) {

        super.setButtonColor(_button, _color);

        if ( colorTrigger.equals("sphere") )
        {
        	int[] selected = surfaceList.getSelectedIndices();
        	DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        	for ( int i = 0; i < selected.length; i++ )
        	{
        		parent.setAnnotationVOIColor( (String)kList.elementAt( selected[i] ),
        				new ColorRGB( _color.getRed()/255.0f, 
        						_color.getGreen()/255.0f,
        						_color.getBlue()/255.0f ) );
        	}
        }
        else if ( colorTrigger.equals("neurite") )
        {
        	int index = neuriteTabbedPane.getSelectedIndex();
        	if ( index == -1 )
        	{
        		return;
        	}
        	String neuriteName = neuriteTabbedPane.getTitleAt(index);
        	parent.setNeuriteColor( neuriteName,
        			new ColorRGB( _color.getRed()/255.0f, 
        					_color.getGreen()/255.0f,
        					_color.getBlue()/255.0f ) );
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

	public void updateFonts( VOIText inputText )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			VOI textVOI = parent.getSelectedVOI( (String)kList.elementAt( selected[i] ) );
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
			
			if ( i == 0 )
			{
				fontButton.setBackground( inputText.getColor() );
			}
		}
	}
	
	@Override
	public void valueChanged(ListSelectionEvent e) {
		if ( e.getSource() == surfaceList )
		{
	        int[] selected = surfaceList.getSelectedIndices();
	        if ( selected != null )
	        {
	        	if ( selected.length > 0 )
	        	{
	                DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
	                boolean[] display = new boolean[1];
	                Color[] color = new Color[1];
	                float[] diameter = new float[1];
	                boolean[] labelDisplay = new boolean[1];
	                Color[] labelColor = new Color[1];
	        		parent.getAnnotationInfo( (String)kList.elementAt( selected[0] ), display, color, diameter, labelDisplay, labelColor );
	        		if ( color[0].equals(Color.white) )
	        		{
	        			sphereColorButton.setBackground(defaultButtonColor);
	        		}
	        		else
	        		{
	        			sphereColorButton.setBackground(color[0]);
	        		}
	        		displaySurface.setSelected(display[0]);
	        		displayLabel.setSelected(labelDisplay[0]);
	        		sphereDiameter.setText( String.valueOf(diameter[0]) );
	        		if ( labelColor[0].equals(Color.white) )
	        		{
	        			fontButton.setBackground(defaultButtonColor);
	        		}
	        		else
	        		{
	        			fontButton.setBackground(labelColor[0]);
	        		}
	        	}
	        }
		}
	}

	private void addList( )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
		int index = neuriteTabbedPane.getSelectedIndex();
        DefaultListModel listModel = (DefaultListModel)neuriteList.elementAt(index).getModel();
        int modelIndex = listModel.size();
        for ( int i = 0; i < selected.length; i++ )
        {
        	listModel.add(modelIndex++, kList.elementAt( selected[i] ));
        }
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
	}

	private void displaySelected( boolean display )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			parent.setDisplayAnnotation( (String)kList.elementAt( selected[i] ), display );
		}
	}

	private void displaySelectedLabel( boolean display )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			parent.setDisplayAnnotationLabel( (String)kList.elementAt( selected[i] ), display );
		}
	}

	private void init( Vector<String> annotations, int timeSteps ) {
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());

        JPanel surfacePanel = new JPanel();
        displaySurface = new JCheckBox("display", true);
        displaySurface.addActionListener(this);
        displaySurface.setActionCommand("display");

        sphereColorButton = new JButton("color");
        sphereColorButton.addActionListener(this);
        sphereColorButton.setActionCommand("color");
        defaultButtonColor = sphereColorButton.getBackground();

        sphereDiameter = new JTextField("1.0");
//        diameter.addFocusListener( this );
        sphereDiameter.addKeyListener( this );

        surfacePanel.add( new JLabel("Annotation: " ) );
        surfacePanel.add(displaySurface);
        surfacePanel.add(sphereColorButton);
        surfacePanel.add( new JLabel("diameter (0.1-2.0): " ) );
        surfacePanel.add(sphereDiameter);
        


        JPanel labelPanel = new JPanel();
        displayLabel = new JCheckBox("display", true);
        displayLabel.addActionListener(this);
        displayLabel.setActionCommand("displayLabel");

        fontButton = new JButton("text options");
        fontButton.addActionListener(this);
        fontButton.setActionCommand("text");
        
        labelPanel.add( new JLabel("Label: " ) );
        labelPanel.add(displayLabel);
        labelPanel.add(fontButton);
        
        JPanel displayOptions = new JPanel(new BorderLayout());
        displayOptions.add( surfacePanel, BorderLayout.NORTH );
        displayOptions.add( labelPanel, BorderLayout.SOUTH );
        
        // list panel for surface filenames
        surfaceList = new JList(new DefaultListModel());
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
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        for ( int i = 0; i < names.length; i++ )
        {
        	kList.add(i, names[i]);
        }
        
        
        
        
        
        
        getContentPane().add(mainPanel);
        pack();
    }

	private JPanel makeNeuriteList( String[] names )
	{
		if ( displayNeurite == null )
		{
			displayNeurite = new Vector<JCheckBox>();
			diameterNeurite = new Vector<JTextField>();
			neuriteList = new Vector<JList>();
			neuriteColorButton = new Vector<JButton>();
		}
		
		JPanel surfacePanel = new JPanel();
		JCheckBox neuriteCheck = new JCheckBox("displayNeurite", false);
		neuriteCheck.addActionListener(this);
		neuriteCheck.setActionCommand("displayNeurite");
        displayNeurite.add( neuriteCheck );

        JButton colorButton = new JButton("color");
        colorButton.addActionListener(this);
        colorButton.setActionCommand("colorNeurite");
        neuriteColorButton.add(colorButton);

        JTextField neuriteText = new JTextField("1.0");
        neuriteText.setEnabled(false);
        neuriteText.addKeyListener( this );
        diameterNeurite.add(neuriteText);

        surfacePanel.add( new JLabel("Neurite: " ) );
        surfacePanel.add(neuriteCheck);
        surfacePanel.add(colorButton);
//        surfacePanel.add( new JLabel("set diameter (0.1-2.0): " ) );
//        surfacePanel.add(diameterNeurite.elementAt(index));
        

        JPanel displayOptions = new JPanel(new BorderLayout());
        displayOptions.add( surfacePanel, BorderLayout.NORTH );

        // list panel for surface filenames
        JList list = new JList(new DefaultListModel());
        list.addListSelectionListener(this);
        list.addMouseListener( this );
        DefaultListModel listModel = (DefaultListModel)list.getModel();
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
        dualPanel.add(list);
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

	private int neuriteCount = 0;
	private void newList( )
	{		
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        String[] names = new String[selected.length];
		for ( int i = 0; i < selected.length; i++ )
		{
			names[i] = (String)kList.elementAt( selected[i] );
		}

        int index = neuriteCount++;
        neuriteTabbedPane.addTab("Neurite Path" + (index + 1), makeNeuriteList(names));	
        neuriteTabbedPane.setSelectedIndex( neuriteTabbedPane.getComponentCount() - 1);
	}

	private void setDiameter( float value )
	{
        int[] selected = surfaceList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
		for ( int i = 0; i < selected.length; i++ )
		{
			parent.setAnnotationDiameter( (String)kList.elementAt( selected[i] ), value );
		}
	}

	private void setSelectedFonts()
	{
        int[] selected = surfaceList.getSelectedIndices();
        if ( selected.length <= 0 )
        {
        	return;
        }
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
		VOI text = parent.getSelectedVOI( (String)kList.elementAt( selected[0] ) );
		if ( text == null )
		{
			return;
		}
		new JDialogVolumeAnnotation( parent.getImage(), text, 0, true, false, false, this);
	}

	private void updateNeurites()
	{
		int index = neuriteTabbedPane.getSelectedIndex();
		if ( index == -1 )
		{
			return;
		}
		JList list = neuriteList.elementAt(index);
        DefaultListModel kList = (DefaultListModel)list.getModel();
        String[] names = new String[ kList.size() ];
        for ( int i = 0; i < kList.size(); i++ )
        {
        	names[i] = (String)kList.elementAt( i );
//        	System.err.println( names[i] );
        }

		String neuriteName = neuriteTabbedPane.getTitleAt(index);
		Color color = neuriteColorButton.elementAt(index).getBackground();
		ColorRGB colorRGB = new ColorRGB( color.getRed()/255f, color.getGreen()/255f, color.getBlue()/255f );
		parent.addNeurite( neuriteName, names, colorRGB );

		parent.displayNeurite( neuriteName, displayNeurite.elementAt(index).isSelected() );
	}

}
