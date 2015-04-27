package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;

import gov.nih.mipav.view.CustomUIBuilder.UIParams;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;

import java.awt.*;
import java.awt.event.*;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;


public class JPanelAnnotationAnimation extends JInterfaceBase implements ChangeListener, ListSelectionListener, KeyListener, MouseListener {

    // private JCheckBox[] checkboxList;
    // private JButton[] colorButtonList;
    // private JCheckBox[] labelCheckList;
    // private JButton[] labelColorList;
    private JSlider annimationSlider;

    private JLabel timeLabel;

    private final VolumeTriPlanarRender parent;

    /** Scroll pane. */
    private JScrollPane scroller;

    public JPanelAnnotationAnimation(final VolumeTriPlanarRender parent, final int timeSteps, final Vector<String> annotationNames) {
        super();
        this.parent = parent;
        // init( timeSteps, annotationNames );
        init(annotationNames, timeSteps);
    }

    // private void init( int timeSteps, Vector<String> annotations )
    // {
    // String[] names = new String[annotations.size()];
    // for ( int i = 0; i < annotations.size(); i++ )
    // {
    // names[i] = annotations.elementAt(i);
    // }
    //
    // Arrays.sort( names );
    // int size = annotations.size();
    // JPanel annotationPanel = new JPanel();
    // JPanel labelsPanel = new JPanel();
    // annotationPanel.setLayout(new GridBagLayout());
    // annotationPanel.setForeground(Color.white);
    // annotationPanel.setBackground(Color.white);
    //
    // labelsPanel.setLayout(new GridBagLayout());
    // labelsPanel.setForeground(Color.white);
    // labelsPanel.setBackground(Color.white);
    //
    // checkboxList = new JCheckBox[size];
    // colorButtonList = new JButton[size];
    // labelCheckList = new JCheckBox[size];
    // labelColorList = new JButton[size];
    //
    // GridBagConstraints gbc = new GridBagConstraints();
    // gbc.gridx = 0;
    // gbc.gridy = 0;
    // // gbc.gridwidth = 1;
    // // gbc.gridheight = 1;
    //
    // for (int i = 0; i < size; i++) { // place nSlices of check options for user and give them a name
    // checkboxList[i] = new JCheckBox( names[i], true );
    // checkboxList[i].addActionListener(this);
    // checkboxList[i].setBackground(Color.white);
    // annotationPanel.add(checkboxList[i], gbc); gbc.gridx++;
    //
    // colorButtonList[i] = new JButton();
    // colorButtonList[i].addActionListener(this);
    // colorButtonList[i].setMinimumSize(new Dimension(30, 30));
    // colorButtonList[i].setMaximumSize(new Dimension(30, 30));
    // colorButtonList[i].setPreferredSize(new Dimension(30, 30));
    // annotationPanel.add(colorButtonList[i], gbc); gbc.gridx++;
    // // annotationPanel.add(new JLabel(" "), gbc); gbc.gridx++;
    //
    //
    // gbc.gridx = 0;
    // labelCheckList[i] = new JCheckBox( "show label", true );
    // labelCheckList[i].addActionListener(this);
    // labelCheckList[i].setBackground(Color.white);
    // labelsPanel.add(labelCheckList[i], gbc); gbc.gridx++;
    //
    // labelColorList[i] = new JButton();
    // labelColorList[i].addActionListener(this);
    // labelColorList[i].setMinimumSize(new Dimension(30, 30));
    // labelColorList[i].setMaximumSize(new Dimension(30, 30));
    // labelColorList[i].setPreferredSize(new Dimension(30, 30));
    // labelsPanel.add(labelColorList[i], gbc); gbc.gridx++;
    // // labelsPanel.add(new JLabel(" "), gbc); gbc.gridx++;
    //
    // gbc.gridx = 0;
    // gbc.gridy++;
    // }
    // JPanel annotationCheckPanel = new JPanel(new GridBagLayout());
    //
    // // make check & uncheck buttons for the panel--place inside the above border
    // JButton checkButton = new JButton("Select all");
    // checkButton.setPreferredSize(new Dimension(95, 30));
    // checkButton.setMinimumSize(new Dimension(95, 30));
    // annotationCheckPanel.add(checkButton, gbc);
    // checkButton.addActionListener(this);
    // checkButton.setActionCommand("SelectAll");
    //
    // gbc.gridx = 1;
    // JButton unCheckButton = new JButton("Clear");
    // unCheckButton.setPreferredSize(new Dimension(95, 30));
    // unCheckButton.setMinimumSize(new Dimension(95, 30));
    // unCheckButton.addActionListener(this);
    // unCheckButton.setActionCommand("ClearAll");
    // annotationCheckPanel.add(unCheckButton, gbc);
    //
    //
    // JPanel labelCheckPanel = new JPanel(new GridBagLayout());
    // gbc = new GridBagConstraints();
    //
    // gbc.gridx = 0;
    // gbc.gridy = 0;
    // gbc.gridwidth = 1;
    // gbc.gridheight = 1;
    //
    // // make check & uncheck buttons for the panel--place inside the above border
    // checkButton = new JButton("Select all");
    // checkButton.setPreferredSize(new Dimension(95, 30));
    // checkButton.setMinimumSize(new Dimension(95, 30));
    // labelCheckPanel.add(checkButton, gbc);
    // checkButton.addActionListener(this);
    // checkButton.setActionCommand("SelectAllLabels");
    //
    // gbc.gridx = 1;
    // unCheckButton = new JButton("Clear");
    // unCheckButton.setPreferredSize(new Dimension(95, 30));
    // unCheckButton.setMinimumSize(new Dimension(95, 30));
    // unCheckButton.addActionListener(this);
    // unCheckButton.setActionCommand("ClearAllLabels");
    // labelCheckPanel.add(unCheckButton, gbc);
    //
    //
    // JPanel annotationListPanel = new JPanel(new BorderLayout());
    // annotationListPanel.add(annotationPanel, BorderLayout.NORTH);
    // annotationListPanel.add(annotationCheckPanel, BorderLayout.CENTER);
    //
    // JPanel annotationLlabelPanel = new JPanel(new BorderLayout());
    // annotationLlabelPanel.add(labelsPanel, BorderLayout.NORTH);
    // annotationLlabelPanel.add(labelCheckPanel, BorderLayout.CENTER);
    //
    // JPanel dualPanel = new JPanel( new GridLayout(1,2) );
    // dualPanel.add(annotationListPanel);
    // dualPanel.add(annotationLlabelPanel);
    //
    //
    // JButton playButton = new JButton("Play");
    // playButton.addActionListener(this);
    // playButton.setActionCommand("Play");
    // playButton.setFont(MipavUtil.font12B);
    // playButton.setPreferredSize(MipavUtil.defaultButtonSize);
    //
    // JButton stopButton = new JButton("Stop");
    // stopButton.addActionListener(this);
    // stopButton.setActionCommand("Stop");
    // stopButton.setFont(MipavUtil.font12B);
    // stopButton.setPreferredSize(MipavUtil.defaultButtonSize);
    //
    // JButton recordButton = new JButton("Record");
    // recordButton.addActionListener(this);
    // recordButton.setActionCommand("Record");
    // recordButton.setFont(MipavUtil.font12B);
    // recordButton.setPreferredSize(MipavUtil.defaultButtonSize);
    //
    //
    // JLabel timeSlice = new JLabel("Time: ");
    // timeLabel = new JLabel("0");
    // annimationSlider = new JSlider(0, timeSteps-1, 0 );
    // annimationSlider.setMajorTickSpacing(1);
    // // annimationSlider.setMinorTickSpacing(1);
    // // annimationSlider.setPaintTicks(true);
    // annimationSlider.addChangeListener(this);
    // // annimationSlider.setPaintLabels(true);
    // annimationSlider.setSnapToTicks(true);
    // annimationSlider.setEnabled(true);
    //
    //
    // GridBagConstraints kGBC = new GridBagConstraints();
    // GridBagLayout kGrid = new GridBagLayout();
    // kGBC.gridx = 0;
    // kGBC.gridy = 0;
    //
    // JPanel kAnimatePanel = new JPanel(kGrid);
    // kAnimatePanel.setBorder(buildTitledBorder("Animation"));
    // kAnimatePanel.add( playButton, kGBC ); kGBC.gridx++;
    // kAnimatePanel.add( stopButton, kGBC ); kGBC.gridx++;
    // kAnimatePanel.add( recordButton, kGBC ); kGBC.gridx++;
    // kGBC.gridx = 0;
    // kGBC.gridy = 1;
    //
    // kAnimatePanel.add( timeSlice, kGBC ); kGBC.gridx++;
    // kAnimatePanel.add( annimationSlider, kGBC ); kGBC.gridx++;
    // kAnimatePanel.add( timeLabel, kGBC ); kGBC.gridx++;
    //
    //
    // JPanel interfacePanel = new JPanel(new BorderLayout());
    // interfacePanel.add(dualPanel, BorderLayout.NORTH);
    // interfacePanel.add(kAnimatePanel, BorderLayout.CENTER);
    //
    //
    //
    // // make the list scroll if there are enough checkboxes
    // scroller = new JScrollPane(interfacePanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
    // JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    //
    // mainPanel = new JPanel(new BorderLayout());
    // mainPanel.add(scroller, BorderLayout.NORTH);
    //
    // getContentPane().add(mainPanel);
    // pack();
    // }

    private JTabbedPane neuriteTabbedPane;

    private JCheckBox displaySurface;

    private JCheckBox displayLabel;

    private JList surfaceList;

    private Vector<JList> neuriteList;

    private JTextField diameter;

    private Vector<JCheckBox> displayNeurite;

    private Vector<JTextField> diameterNeurite;

    private void init(final Vector<String> annotations, final int timeSteps) {
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        final JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());

        final JPanel surfacePanel = new JPanel();
        displaySurface = new JCheckBox("display", true);
        displaySurface.addActionListener(this);
        displaySurface.setActionCommand("display");

        final JButton colorButton = new JButton("color");
        colorButton.addActionListener(this);
        colorButton.setActionCommand("color");

        diameter = new JTextField("1.0");
        // diameter.addFocusListener( this );
        diameter.addKeyListener(this);

        surfacePanel.add(new JLabel("Annotation: "));
        surfacePanel.add(displaySurface);
        surfacePanel.add(colorButton);
        surfacePanel.add(new JLabel("set diameter (0.1-2.0): "));
        surfacePanel.add(diameter);

        final JPanel labelPanel = new JPanel();
        displayLabel = new JCheckBox("display", true);
        displayLabel.addActionListener(this);
        displayLabel.setActionCommand("displayLabel");

        final JButton fontButton = new JButton("text options");
        fontButton.addActionListener(this);
        fontButton.setActionCommand("text");

        labelPanel.add(new JLabel("Label: "));
        labelPanel.add(displayLabel);
        labelPanel.add(fontButton);

        final JPanel displayOptions = new JPanel(new BorderLayout());
        displayOptions.add(surfacePanel, BorderLayout.NORTH);
        displayOptions.add(labelPanel, BorderLayout.SOUTH);

        // list panel for surface filenames
        surfaceList = new JList(new DefaultListModel());
        surfaceList.addListSelectionListener(this);
        surfaceList.addMouseListener(this);

        final JScrollPane kScrollPane = new JScrollPane(surfaceList);
        final JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        final JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(displayOptions, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("Annotation list"));

        // ////////////////////////////////////////////////////////////////

        neuriteTabbedPane = new JTabbedPane();

        // ///////////////////////////////////////////////////////////////
        final JButton playButton = new JButton("Play");
        playButton.addActionListener(this);
        playButton.setActionCommand("Play");
        playButton.setFont(MipavUtil.font12B);
        playButton.setPreferredSize(MipavUtil.defaultButtonSize);

        final JButton stopButton = new JButton("Stop");
        stopButton.addActionListener(this);
        stopButton.setActionCommand("Stop");
        stopButton.setFont(MipavUtil.font12B);
        stopButton.setPreferredSize(MipavUtil.defaultButtonSize);

        final JButton recordButton = new JButton("Record");
        recordButton.addActionListener(this);
        recordButton.setActionCommand("Record");
        recordButton.setFont(MipavUtil.font12B);
        recordButton.setPreferredSize(MipavUtil.defaultButtonSize);

        final JLabel timeSlice = new JLabel("Time: ");
        timeLabel = new JLabel("0");
        annimationSlider = new JSlider(0, timeSteps - 1, 0);
        annimationSlider.setMajorTickSpacing(1);
        // annimationSlider.setMinorTickSpacing(1);
        // annimationSlider.setPaintTicks(true);
        annimationSlider.addChangeListener(this);
        // annimationSlider.setPaintLabels(true);
        annimationSlider.setSnapToTicks(true);
        annimationSlider.setEnabled(true);

        final GridBagConstraints kGBC = new GridBagConstraints();
        final GridBagLayout kGrid = new GridBagLayout();
        kGBC.gridx = 0;
        kGBC.gridy = 0;

        final JPanel kAnimatePanel = new JPanel(kGrid);
        kAnimatePanel.setBorder(buildTitledBorder("Animation"));
        kAnimatePanel.add(playButton, kGBC);
        kGBC.gridx++;
        kAnimatePanel.add(stopButton, kGBC);
        kGBC.gridx++;
        kAnimatePanel.add(recordButton, kGBC);
        kGBC.gridx++;
        kGBC.gridx = 0;
        kGBC.gridy = 1;

        kAnimatePanel.add(timeSlice, kGBC);
        kGBC.gridx++;
        kAnimatePanel.add(annimationSlider, kGBC);
        kGBC.gridx++;
        kAnimatePanel.add(timeLabel, kGBC);
        kGBC.gridx++;

        mainScrollPanel.add(listPanel, BorderLayout.NORTH);
        mainScrollPanel.add(neuriteTabbedPane, BorderLayout.CENTER);
        mainScrollPanel.add(kAnimatePanel, BorderLayout.SOUTH);
        mainPanel.add(scroller, BorderLayout.CENTER);

        final String[] names = new String[annotations.size()];
        for (int i = 0; i < annotations.size(); i++) {
            names[i] = annotations.elementAt(i);
        }

        Arrays.sort(names);
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        for (int i = 0; i < names.length; i++) {
            kList.add(i, names[i]);
        }

        getContentPane().add(mainPanel);
        pack();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();
        if (command.equals("Play") || command.equals("Stop")) {
            parent.startStopVOIAnimation();
        } else if (command.equals("Record")) {
            parent.startRecording();
        } else if (command.equals("display")) {
            displaySelected(displaySurface.isSelected());
        } else if (command.equals("color")) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(), new CancelListener());
        } else if (command.equals("displayLabel")) {
            displaySelectedLabel(displayLabel.isSelected());
        } else if (command.equals("text")) {
            setSelectedFonts();
        } else if (command.equals("newlist")) {
            newList();
        } else if (command.equals("addlist")) {
            addList();
            updateNeurites();
        } else if (command.equals("moveUp")) {
            final int index = neuriteTabbedPane.getSelectedIndex();
            if (index == -1) {
                return;
            }
            final JList list = neuriteList.elementAt(index);
            final int[] selected = list.getSelectedIndices();
            if (selected.length == 1) {
                if (selected[0] > 0) {
                    final DefaultListModel kList = (DefaultListModel) list.getModel();
                    final String name = (String) kList.remove(selected[0]);
                    kList.add(selected[0] - 1, name);
                    list.setSelectedIndex(selected[0] - 1);
                    // System.err.println( "moveUp " + name );
                }
            }
            updateNeurites();
        } else if (command.equals("moveDown")) {
            final int index = neuriteTabbedPane.getSelectedIndex();
            if (index == -1) {
                return;
            }
            final JList list = neuriteList.elementAt(index);
            final int[] selected = list.getSelectedIndices();
            if (selected.length == 1) {
                final DefaultListModel kList = (DefaultListModel) list.getModel();
                if ( (selected[0] + 1) < kList.size()) {
                    final String name = (String) kList.remove(selected[0]);
                    kList.add(selected[0] + 1, name);
                    list.setSelectedIndex(selected[0] + 1);
                    // System.err.println( "moveDown " + name );
                }
            }
            updateNeurites();
        } else if (command.equals("moveFirst")) {
            final int index = neuriteTabbedPane.getSelectedIndex();
            if (index == -1) {
                return;
            }
            final JList list = neuriteList.elementAt(index);
            final int[] selected = list.getSelectedIndices();
            if (selected.length == 1) {
                final DefaultListModel kList = (DefaultListModel) list.getModel();
                final String name = (String) kList.remove(selected[0]);
                kList.add(0, name);
                list.setSelectedIndex(0);
                // System.err.println( "moveFirst " + name );
            }
            updateNeurites();
        } else if (command.equals("moveLast")) {
            int index = neuriteTabbedPane.getSelectedIndex();
            if (index == -1) {
                return;
            }
            final JList list = neuriteList.elementAt(index);
            final int[] selected = list.getSelectedIndices();
            if (selected.length == 1) {
                final DefaultListModel kList = (DefaultListModel) list.getModel();
                final String name = (String) kList.remove(selected[0]);
                index = kList.size();
                kList.add(index, name);
                list.setSelectedIndex(index);
                // System.err.println( "moveLast " + name );
            }
            updateNeurites();
        } else if (command.equals("delete")) {
            final int index = neuriteTabbedPane.getSelectedIndex();
            if (index == -1) {
                return;
            }
            final JList list = neuriteList.elementAt(index);
            final int[] selected = list.getSelectedIndices();
            final DefaultListModel kList = (DefaultListModel) list.getModel();
            for (int i = selected.length - 1; i >= 0; i--) {
                final String name = (String) kList.remove(selected[i]);
                // System.err.println( "delete " + name );
            }
            updateNeurites();
        } else if (command.contains("displayNeurite")) {
            updateNeurites();
        }
        // else if ( command.equals("SelectAll") )
        // {
        // for (int i = 0; i < checkboxList.length; i++)
        // {
        // checkboxList[i].removeActionListener(this);
        // checkboxList[i].setSelected(true);
        // parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
        // checkboxList[i].addActionListener(this);
        // }
        // }
        // else if ( command.equals("ClearAll") )
        // {
        // for (int i = 0; i < checkboxList.length; i++)
        // {
        // checkboxList[i].removeActionListener(this);
        // checkboxList[i].setSelected(false);
        // parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
        // checkboxList[i].addActionListener(this);
        // }
        // }
        // else if ( command.equals("SelectAllLabels") )
        // {
        // for (int i = 0; i < labelCheckList.length; i++)
        // {
        // labelCheckList[i].removeActionListener(this);
        // labelCheckList[i].setSelected(true);
        // parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
        // labelCheckList[i].addActionListener(this);
        // }
        // }
        // else if ( command.equals("ClearAllLabels") )
        // {
        // for (int i = 0; i < labelCheckList.length; i++)
        // {
        // labelCheckList[i].removeActionListener(this);
        // labelCheckList[i].setSelected(false);
        // parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
        // labelCheckList[i].addActionListener(this);
        // }
        // }
        // for ( int i = 0; i < checkboxList.length; i++ )
        // {
        // if ( e.getSource() == checkboxList[i] )
        // {
        // parent.setDisplayAnnotation( checkboxList[i].getText(), checkboxList[i].isSelected() );
        // break;
        // }
        // }
        // for ( int i = 0; i < labelCheckList.length; i++ )
        // {
        // if ( e.getSource() == labelCheckList[i] )
        // {
        // parent.setDisplayAnnotationLabel( checkboxList[i].getText(), labelCheckList[i].isSelected() );
        // break;
        // }
        // }
        // for ( int i = 0; i < colorButtonList.length; i++ )
        // {
        // if ( e.getSource() == colorButtonList[i] )
        // {
        // colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(colorButtonList[i]),
        // new CancelListener());
        // break;
        // }
        // }
        // for ( int i = 0; i < labelColorList.length; i++ )
        // {
        // if ( e.getSource() == labelColorList[i] )
        // {
        // colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener(labelColorList[i]),
        // new CancelListener());
        // break;
        // }
        // }
    }

    private void displaySelected(final boolean display) {
        final int[] selected = surfaceList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        for (int i = 0; i < selected.length; i++) {
            parent.setDisplayAnnotation((String) kList.elementAt(selected[i]), display);
        }
    }

    private void setDiameter(final float value) {
        final int[] selected = surfaceList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        for (int i = 0; i < selected.length; i++) {
            parent.setAnnotationDiameter((String) kList.elementAt(selected[i]), value);
        }
    }

    private void newList() {
        final int[] selected = surfaceList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        final String[] names = new String[selected.length];
        for (int i = 0; i < selected.length; i++) {
            names[i] = (String) kList.elementAt(selected[i]);
        }

        final int index = neuriteTabbedPane.getTabCount();
        neuriteTabbedPane.addTab("Neurite Path" + (index + 1), makeNeuriteList(index, names));
    }

    private void addList() {
        final int[] selected = surfaceList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        final int index = neuriteTabbedPane.getSelectedIndex();
        final DefaultListModel listModel = (DefaultListModel) neuriteList.elementAt(index).getModel();
        int modelIndex = listModel.size();
        for (int i = 0; i < selected.length; i++) {
            listModel.add(modelIndex++, kList.elementAt(selected[i]));
        }
    }

    private void updateNeurites() {
        final int index = neuriteTabbedPane.getSelectedIndex();
        if (index == -1) {
            return;
        }
        final JList list = neuriteList.elementAt(index);
        final DefaultListModel kList = (DefaultListModel) list.getModel();
        final String[] names = new String[kList.size()];
        for (int i = 0; i < kList.size(); i++) {
            names[i] = (String) kList.elementAt(i);
            // System.err.println( names[i] );
        }

        final String neuriteName = neuriteTabbedPane.getTitleAt(index);
        parent.addNeurite(neuriteName, names);

        parent.displayNeurite(neuriteName, displayNeurite.elementAt(index).isSelected());
    }

    private JPanel makeNeuriteList(final int index, final String[] names) {
        if (displayNeurite == null) {
            displayNeurite = new Vector<JCheckBox>();
            diameterNeurite = new Vector<JTextField>();
            neuriteList = new Vector<JList>();
        }

        final JPanel surfacePanel = new JPanel();
        final JCheckBox neuriteCheck = new JCheckBox("displayNeurite", false);
        // neuriteCheck.setEnabled(false);
        neuriteCheck.addActionListener(this);
        neuriteCheck.setActionCommand("displayNeurite" + index);
        displayNeurite.add(neuriteCheck);

        final JButton colorButton = new JButton("color");
        colorButton.setEnabled(false);
        colorButton.addActionListener(this);
        colorButton.setActionCommand("colorNeurite" + index);

        final JTextField neuriteText = new JTextField("1.0");
        neuriteText.setEnabled(false);
        neuriteText.addKeyListener(this);
        diameterNeurite.add(neuriteText);

        surfacePanel.add(new JLabel("Neurite: "));
        surfacePanel.add(displayNeurite.elementAt(index));
        surfacePanel.add(colorButton);
        surfacePanel.add(new JLabel("set diameter (0.1-2.0): "));
        surfacePanel.add(diameterNeurite.elementAt(index));

        final JPanel displayOptions = new JPanel(new BorderLayout());
        displayOptions.add(surfacePanel, BorderLayout.NORTH);

        // list panel for surface filenames
        final JList list = new JList(new DefaultListModel());
        list.addListSelectionListener(this);
        list.addMouseListener(this);
        final DefaultListModel listModel = (DefaultListModel) list.getModel();
        for (int i = 0; i < names.length; i++) {
            listModel.add(i, names[i]);
        }
        neuriteList.add(list);

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        final JPanel listOptions = new JPanel(new GridBagLayout());
        final JButton up = new JButton("move up");
        up.setPreferredSize(new Dimension(100, 30));
        up.addActionListener(this);
        up.setActionCommand("moveUp");
        listOptions.add(up, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        final JButton down = new JButton("move down");
        down.setPreferredSize(new Dimension(100, 30));
        down.addActionListener(this);
        down.setActionCommand("moveDown");
        listOptions.add(down, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        final JButton first = new JButton("move first");
        first.setPreferredSize(new Dimension(100, 30));
        first.addActionListener(this);
        first.setActionCommand("moveFirst");
        listOptions.add(first, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        ;
        final JButton last = new JButton("move last");
        last.setPreferredSize(new Dimension(100, 30));
        last.addActionListener(this);
        last.setActionCommand("moveLast");
        listOptions.add(last, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        final JButton delete = new JButton("delete");
        delete.setPreferredSize(new Dimension(100, 30));
        delete.addActionListener(this);
        delete.setActionCommand("delete");
        listOptions.add(delete, gbc);

        final JPanel dualPanel = new JPanel(new GridLayout(1, 2));
        dualPanel.add(neuriteList.elementAt(index));
        dualPanel.add(listOptions);

        final JScrollPane kScrollPane = new JScrollPane(dualPanel);
        final JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        final JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(displayOptions, BorderLayout.SOUTH);

        return listPanel;
    }

    private void displaySelectedLabel(final boolean display) {
        final int[] selected = surfaceList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        for (int i = 0; i < selected.length; i++) {
            parent.setDisplayAnnotationLabel((String) kList.elementAt(selected[i]), display);
        }
    }

    private void setSelectedFonts() {
        final int[] selected = surfaceList.getSelectedIndices();
        if (selected.length <= 0) {
            return;
        }
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        final VOI text = parent.getSelectedVOI((String) kList.elementAt(selected[0]));
        if (text == null) {
            return;
        }
        new JDialogVolumeAnnotation(parent.getImage(), text, 0, true, false, false, this);
    }

    public void updateFonts(final VOIText inputText) {
        final int[] selected = surfaceList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        for (int i = 0; i < selected.length; i++) {
            final VOI textVOI = parent.getSelectedVOI((String) kList.elementAt(selected[i]));
            final VOIText text = (VOIText) textVOI.getCurves().elementAt(0);

            if (text != inputText) {
                text.setFontSize(inputText.getFontSize());
                text.setFontDescriptors(inputText.getFontDescriptors());
                text.setFontName(inputText.getFontName());
                text.setColor(inputText.getColor());
                text.setBackgroundColor(inputText.getBackgroundColor());
                text.setUseMarker(inputText.useMarker());
                text.updateText();
            }
        }
    }

    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     * 
     * @param panelWidth width
     * @param frameHeight height
     */
    public void resizePanel(final int panelWidth, final int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton,
     * java.awt.Color)
     */
    @Override
    public void setButtonColor(final JButton _button, final Color _color) {

        super.setButtonColor(_button, _color);

        final int[] selected = surfaceList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) surfaceList.getModel();
        for (int i = 0; i < selected.length; i++) {
            parent.setAnnotationVOIColor((String) kList.elementAt(selected[i]),
                    new ColorRGB(_color.getRed() / 255.0f, _color.getGreen() / 255.0f, _color.getBlue() / 255.0f));
        }
        //
        // if ( parent != null )
        // {
        // for (int i = 0; i < colorButtonList.length; i++)
        // {
        // if ( colorButtonList[i] == _button )
        // {
        // parent.setAnnotationVOIColor( checkboxList[i].getText(),
        // new ColorRGB( _color.getRed()/255.0f,
        // _color.getGreen()/255.0f,
        // _color.getBlue()/255.0f ) );
        // }
        // if ( labelColorList[i] == _button )
        // {
        // parent.setAnnotationLabelColor( checkboxList[i].getText(),
        // new ColorRGBA( _color.getRed()/255.0f,
        // _color.getGreen()/255.0f,
        // _color.getBlue()/255.0f, 1 ) );
        // }
        // }
        // }
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
         * @param _button DOCUMENT ME!
         */
        OkColorListener(final JButton _button) {
            super();
            button = _button;
        }

        OkColorListener() {
            super();
        }

        /**
         * Get color from chooser and set button and color.
         * 
         * @param e Event that triggered function.
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final Color color = colorChooser.getColor();
            setButtonColor(button, color);
        }
    }

    @Override
    public void stateChanged(final ChangeEvent e) {
        if (e.getSource() == annimationSlider) {
            parent.annotationVOIsUpdate(annimationSlider.getValue());
            timeLabel.setText(String.valueOf(annimationSlider.getValue()));
        }
    }

    public void setAnnimationSlider(final int value) {
        annimationSlider.removeChangeListener(this);
        annimationSlider.setValue(value);
        timeLabel.setText(String.valueOf(annimationSlider.getValue()));
        annimationSlider.addChangeListener(this);
    }

    @Override
    public void valueChanged(final ListSelectionEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void keyTyped(final KeyEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void keyPressed(final KeyEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void keyReleased(final KeyEvent e) {
        if (e.getKeyCode() == KeyEvent.VK_ENTER) {
            // System.err.println( diameter.getText() );
            if ( !JDialogBase.testParameter(diameter.getText(), 0.1, 2.0)) {
                diameter.requestFocus();
                diameter.selectAll();
            } else {
                setDiameter(Float.valueOf(diameter.getText()));
            }
        }
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        if (e.isPopupTrigger()) {
            createPopup(e);
        }
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        if (e.isPopupTrigger()) {
            createPopup(e);
        }
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        if (e.isPopupTrigger()) {
            createPopup(e);
        }
    }

    @Override
    public void mouseEntered(final MouseEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void mouseExited(final MouseEvent e) {
        // TODO Auto-generated method stub

    }

    private void createPopup(final MouseEvent e) {
        if (e.getSource() == surfaceList) {
            final JPopupMenu popup = new JPopupMenu();
            popup.add(ViewMenuBuilder.buildMenuItem(new UIParams("New List", "newlist", UIParams.INVALID_MNEMONIC, null, null), this, false));
            popup.add(ViewMenuBuilder.buildMenuItem(new UIParams("Add to List", "addlist", UIParams.INVALID_MNEMONIC, null, null), this, false));
            popup.show(surfaceList, e.getX(), e.getY());
        }
        // else
        // {
        // int index = neuriteTabbedPane.getSelectedIndex();
        // if ( index == -1 )
        // {
        // return;
        // }
        // JList list = neuriteList.elementAt(index);
        // if ( e.getSource() == list )
        // {
        // JPopupMenu popup = new JPopupMenu();
        // popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move Up", "moveUp", UIParams.INVALID_MNEMONIC, null,
        // null), this, false) );
        // popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move Down", "moveDown", UIParams.INVALID_MNEMONIC,
        // null, null), this, false) );
        // popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move First", "moveFirst", UIParams.INVALID_MNEMONIC,
        // null, null), this, false) );
        // popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Move Last", "moveLast", UIParams.INVALID_MNEMONIC,
        // null, null), this, false) );
        // popup.add( ViewMenuBuilder.buildMenuItem(new UIParams("Delete", "delete", UIParams.INVALID_MNEMONIC, null,
        // null), this, false) );
        // popup.show(list, e.getX(), e.getY());
        // }
        // }
    }

}
