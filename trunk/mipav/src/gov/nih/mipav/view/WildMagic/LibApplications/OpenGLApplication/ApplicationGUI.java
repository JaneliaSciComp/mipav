// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
import java.util.Enumeration;
import java.util.Hashtable;

import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class ApplicationGUI implements ActionListener
{

    /**
     * Constructs an ApplicationGUI object.
     */
    public ApplicationGUI ()
    {
        m_kShaderMap = new Hashtable<String,Vector<UserConstant>>();
    }
    
    /**
     * Delete memory.
     */
    public void dispose()
    {
        close();
        m_kShaderMap = null;
    }

    /** Adds a user-parameter to the shader with the specified shader name. A
     * label and text field user-interface components are created for the
     * shader parameter.
     * @param kShaderName, the name of the Shader to which the parameter belongs.
     * @param kVar, the UserConstant shader parameter
     */
    private void AddUserVariable ( String kShaderName, UserConstant kVar )
    {
        Vector<UserConstant> kUserVariables = m_kShaderMap.get(kShaderName);
        if ( kUserVariables == null )
        {
            kUserVariables = new Vector<UserConstant>();
            kUserVariables.add(kVar);
            m_kShaderMap.put(kShaderName, kUserVariables);
        }
        else
        {
            kUserVariables.add(kVar);
        }
    }

    /**
     * Displays the Application Shader GUI
     */
    public void Display ()
    {
        if ( ms_bInit == true )
        {
            if ( m_kFrame != null )
            {
                m_kFrame.setVisible(true);
            }            
            return;
        }

        m_kFrame = new JFrame( "Shader Parameters" );
        JPanel kMainPanel = new JPanel( new GridBagLayout() );
        GridBagConstraints kMainGBC = new GridBagConstraints();
        kMainGBC.anchor = GridBagConstraints.WEST;
        kMainGBC.gridx = 0;
        kMainGBC.gridy = 0;
        
        for (Enumeration kE = m_kShaderMap.keys() ; kE.hasMoreElements() ;) {
            String kKey = (String)kE.nextElement();
            Vector<UserConstant> kUserVariables = m_kShaderMap.get(kKey);

            JPanel kPanel = new JPanel( new GridBagLayout() );
            kPanel.setBorder( new TitledBorder( kKey + " Shader Parameters" ) );
            Dimension minimumSize = new Dimension(400, 400);
            kPanel.setMinimumSize(minimumSize);
            GridBagConstraints kGBC = new GridBagConstraints();
            kGBC.anchor = GridBagConstraints.WEST;
            kGBC.gridx = 0;
            kGBC.gridy = 0;
            
            
            for ( int i = 0; i < kUserVariables.size(); i++ )
            {
                kGBC.gridx = 0;
                String kName = kUserVariables.get(i).GetName();
                kPanel.add( new JLabel( kName ), kGBC );
                kGBC.gridx++;
                for ( int j = 0; j < kUserVariables.get(i).GetDataSize(); j++ )
                {
                    JTextField kTextField = new JTextField( String.valueOf(kUserVariables.get(i).GetData(j)), 5 );
                    kTextField.setEditable(true);
                    kTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
                    kTextField.addActionListener(this);
                    kTextField.setActionCommand( new String( kName + j ) );
                    kTextField.setEnabled( true );
                    kPanel.add( kTextField, kGBC );
                    kGBC.gridx++;
                }
                kGBC.gridy++;
            }
            kMainPanel.add( kPanel, kMainGBC );
            kMainGBC.gridy++;
        }

        JButton kReloadShaderButton = new JButton( "Reload Shader Program" );
        kReloadShaderButton.addActionListener(this);
        kReloadShaderButton.setActionCommand("reload");
        kMainGBC.gridx = 0;
        kMainGBC.gridy++;
        kMainPanel.add(kReloadShaderButton, kMainGBC );

        m_kFrame.add(kMainPanel);
        m_kFrame.pack();
        m_kFrame.setVisible(true);

        ms_bInit = true;
    }

    /** 
     * Closes the ApplicationGUI frame, and clears all data members.
     */
    public void close()
    {
        if ( m_kFrame != null )
        {
            m_kFrame.setVisible(false);
        }
        if ( m_kShaderMap != null )
        {
            m_kShaderMap.clear();
        }

        m_kParent = null;
        m_kFrame = null;
        ms_bInit = false;
    }
    
    /** 
     * Called when the user changes one of the Shader parameters. The
     * corresponding UserConstant variable is found and the updated value is
     * passed to the rendering system.
     * @param kEvent, the triggering event.
     */
    public void actionPerformed(ActionEvent kEvent) {
        String kCommand = kEvent.getActionCommand();

        if ( kCommand.equals( "reload" ) )
        {
            if ( m_kParent != null )
            {
                m_kShaderMap.clear();
                m_kParent.reloadShaders();
                ms_bInit = false;
                m_kFrame.setVisible(false);
                m_kFrame.dispose();
            }
        }
        else
        {
            for (Enumeration kE = m_kShaderMap.elements() ; kE.hasMoreElements() ;) {
                Vector kUserVariables =(Vector)kE.nextElement();
                for ( int i = 0; i < kUserVariables.size(); i++ )
                {
                    UserConstant kUser = (UserConstant)kUserVariables.get(i);
                    for ( int j = 0; j < kUser.GetDataSize(); j++ )
                    {
                        String kName = new String( kUser.GetName() + j );
                        if ( kCommand.equals( kName ) )
                        {
                            JTextField kTextField = (JTextField)kEvent.getSource();
                            if ( kTextField.getText().length() > 0 )
                            {
                                float fValue = Float.parseFloat( kTextField.getText() );
                                kUser.SetData( j, fValue );
                            }
                        }
                    }
                }
            }
        }
    }

    /** Sets the parent Application
     * @param kParent, the parent Application.
     */
    public void setParent( Application kParent )
    {
        m_kParent = kParent;
    }

    /** 
     * Adds the User-Defined variables from the input Program to the use-interface.
     * @param kProgram Program containing User-Defined shader parameters that will be added to the user-interface.
     */
    public void AddUserVariables( Program kProgram )
    {
        for ( int i = 0; i < kProgram.GetUCQuantity(); i++ )
        {
            AddUserVariable(kProgram.GetName(), kProgram.GetUC(i) );
        }
    }

    /** HashTable maps Shader parameters to the Shader name: */
    private Hashtable<String,Vector<UserConstant>> m_kShaderMap = null;

    /** Initialize the interface once. */
    private boolean ms_bInit = false;

    /** Parent Application */
    private Application m_kParent = null;
    /** Local JFrame */
    private JFrame m_kFrame = null;
}
