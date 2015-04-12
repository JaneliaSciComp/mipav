package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
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
import java.util.Arrays;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;


public class JPanelAnnotationAnimation extends JInterfaceBase implements ChangeListener {

    private JCheckBox[] checkboxList;
    private JButton[] colorButtonList;
    private JCheckBox[] labelCheckList;
    private JButton[] labelColorList;
    private JSlider annimationSlider;
    private JLabel timeLabel;
    private VolumeTriPlanarRender parent;

    /** Scroll pane. */
    private JScrollPane scroller;
    
    public JPanelAnnotationAnimation( VolumeTriPlanarRender parent, int timeSteps, Vector<String> annotationNames )
    {
        super();
        this.parent = parent;
        init( timeSteps, annotationNames );
    }
    
    private void init( int timeSteps, Vector<String> annotations )
    {
    	String[] names = new String[annotations.size()];
    	for ( int i = 0; i < annotations.size(); i++ )
    	{
    		names[i] = annotations.elementAt(i);
    	}
    	
        Arrays.sort( names );
    	int size = annotations.size();
    	JPanel annotationPanel = new JPanel();
    	JPanel labelsPanel = new JPanel();
    	annotationPanel.setLayout(new GridBagLayout());
    	annotationPanel.setForeground(Color.white);
    	annotationPanel.setBackground(Color.white);
    	
    	labelsPanel.setLayout(new GridBagLayout());
    	labelsPanel.setForeground(Color.white);
    	labelsPanel.setBackground(Color.white);
        
        checkboxList = new JCheckBox[size];
        colorButtonList = new JButton[size];
        labelCheckList = new JCheckBox[size];
        labelColorList = new JButton[size];
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
//        gbc.gridwidth = 1;
//        gbc.gridheight = 1;

        for (int i = 0; i < size; i++) { // place nSlices of check options for user and give them a name
            checkboxList[i] = new JCheckBox( names[i], true );
            checkboxList[i].addActionListener(this);
            checkboxList[i].setBackground(Color.white);
            annotationPanel.add(checkboxList[i], gbc); gbc.gridx++;

            colorButtonList[i] = new JButton();
            colorButtonList[i].addActionListener(this);
            colorButtonList[i].setMinimumSize(new Dimension(30, 30));
            colorButtonList[i].setMaximumSize(new Dimension(30, 30));
            colorButtonList[i].setPreferredSize(new Dimension(30, 30));
            annotationPanel.add(colorButtonList[i], gbc); gbc.gridx++;
//            annotationPanel.add(new JLabel(" "), gbc); gbc.gridx++;
            

            gbc.gridx = 0;
            labelCheckList[i] = new JCheckBox( "show label", true );
            labelCheckList[i].addActionListener(this);
            labelCheckList[i].setBackground(Color.white);
            labelsPanel.add(labelCheckList[i], gbc); gbc.gridx++;
            
            labelColorList[i] = new JButton();
            labelColorList[i].addActionListener(this);
            labelColorList[i].setMinimumSize(new Dimension(30, 30));
            labelColorList[i].setMaximumSize(new Dimension(30, 30));
            labelColorList[i].setPreferredSize(new Dimension(30, 30));
            labelsPanel.add(labelColorList[i], gbc); gbc.gridx++;
//            labelsPanel.add(new JLabel(" "), gbc); gbc.gridx++;
            
            gbc.gridx = 0;
            gbc.gridy++;
        }
        JPanel annotationCheckPanel = new JPanel(new GridBagLayout());

        // make check & uncheck buttons for the panel--place inside the above border
        JButton checkButton = new JButton("Select all");
        checkButton.setPreferredSize(new Dimension(95, 30));
        checkButton.setMinimumSize(new Dimension(95, 30));
        annotationCheckPanel.add(checkButton, gbc);
        checkButton.addActionListener(this);
        checkButton.setActionCommand("SelectAll");

        gbc.gridx = 1;
        JButton unCheckButton = new JButton("Clear");
        unCheckButton.setPreferredSize(new Dimension(95, 30));
        unCheckButton.setMinimumSize(new Dimension(95, 30));
        unCheckButton.addActionListener(this);
        unCheckButton.setActionCommand("ClearAll");
        annotationCheckPanel.add(unCheckButton, gbc);
        

        JPanel labelCheckPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;

        // make check & uncheck buttons for the panel--place inside the above border
        checkButton = new JButton("Select all");
        checkButton.setPreferredSize(new Dimension(95, 30));
        checkButton.setMinimumSize(new Dimension(95, 30));
        labelCheckPanel.add(checkButton, gbc);
        checkButton.addActionListener(this);
        checkButton.setActionCommand("SelectAllLabels");

        gbc.gridx = 1;
        unCheckButton = new JButton("Clear");
        unCheckButton.setPreferredSize(new Dimension(95, 30));
        unCheckButton.setMinimumSize(new Dimension(95, 30));
        unCheckButton.addActionListener(this);
        unCheckButton.setActionCommand("ClearAllLabels");
        labelCheckPanel.add(unCheckButton, gbc);


        JPanel annotationListPanel = new JPanel(new BorderLayout());
        annotationListPanel.add(annotationPanel, BorderLayout.NORTH);        
        annotationListPanel.add(annotationCheckPanel, BorderLayout.CENTER);

        JPanel annotationLlabelPanel = new JPanel(new BorderLayout());
        annotationLlabelPanel.add(labelsPanel, BorderLayout.NORTH);        
        annotationLlabelPanel.add(labelCheckPanel, BorderLayout.CENTER);
        
        JPanel dualPanel = new JPanel( new GridLayout(1,2) );
        dualPanel.add(annotationListPanel);
        dualPanel.add(annotationLlabelPanel);        
        

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
                
        
        JPanel interfacePanel = new JPanel(new BorderLayout());        
        interfacePanel.add(dualPanel, BorderLayout.NORTH);
        interfacePanel.add(kAnimatePanel, BorderLayout.CENTER);



        // make the list scroll if there are enough checkboxes
        scroller = new JScrollPane(interfacePanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        mainPanel = new JPanel(new BorderLayout());       
        mainPanel.add(scroller, BorderLayout.NORTH); 
        
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
		else if ( command.equals("SelectAll") )
		{    
			for (int i = 0; i < checkboxList.length; i++)
			{
				checkboxList[i].removeActionListener(this);
				checkboxList[i].setSelected(true);
				parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
				checkboxList[i].addActionListener(this);
			}
		}
		else if ( command.equals("ClearAll") )
		{
			for (int i = 0; i < checkboxList.length; i++)
			{
				checkboxList[i].removeActionListener(this);
				checkboxList[i].setSelected(false);
				parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
				checkboxList[i].addActionListener(this);
			}
		}
		else if ( command.equals("SelectAllLabels") )
		{    
			for (int i = 0; i < labelCheckList.length; i++)
			{
				labelCheckList[i].removeActionListener(this);
				labelCheckList[i].setSelected(true);
				parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
				labelCheckList[i].addActionListener(this);
			}
		}
		else if ( command.equals("ClearAllLabels") )
		{
			for (int i = 0; i < labelCheckList.length; i++)
			{
				labelCheckList[i].removeActionListener(this);
				labelCheckList[i].setSelected(false);
				parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
				labelCheckList[i].addActionListener(this);
			}
		}
		for ( int i = 0; i < checkboxList.length; i++ )
		{
			if ( e.getSource() == checkboxList[i] )
			{
				parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
				break;
			}
		}
		for ( int i = 0; i < labelCheckList.length; i++ )
		{
			if ( e.getSource() == labelCheckList[i] )
			{
				parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
				break;
			}
		}
		for ( int i = 0; i < colorButtonList.length; i++ )
		{
			if ( e.getSource() == colorButtonList[i] )
			{
				colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(colorButtonList[i]),
						new CancelListener());
				break;
			}
		}
		for ( int i = 0; i < labelColorList.length; i++ )
		{
			if ( e.getSource() == labelColorList[i] )
			{
				colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(labelColorList[i]),
						new CancelListener());
				break;
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

        if ( parent != null )
        {
			for (int i = 0; i < colorButtonList.length; i++)
			{
				if ( colorButtonList[i] == _button )
				{
					parent.setAnnotationVOIColor( checkboxList[i].getText(),
							new ColorRGB( _color.getRed()/255.0f, 
									      _color.getGreen()/255.0f,
									      _color.getBlue()/255.0f ) );
				}
				if ( labelColorList[i] == _button )
				{
					parent.setAnnotationLabelColor( checkboxList[i].getText(),
							new ColorRGBA( _color.getRed()/255.0f, 
									      _color.getGreen()/255.0f,
									      _color.getBlue()/255.0f, 1 ) );
				}
            }
        }
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
}
