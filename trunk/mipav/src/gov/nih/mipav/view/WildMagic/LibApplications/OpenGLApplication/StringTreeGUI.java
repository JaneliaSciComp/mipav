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
import java.awt.*;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class StringTreeGUI extends JFrame
    implements TreeSelectionListener
{

    /** Serialization ID */
    private static final long serialVersionUID = 1L;


    /** Creates and new StringTreeGUI with the StringTree parameter.
     * @param kStringTree, the tree representing the scene-graph object.
     */
    public StringTreeGUI( StringTree kStringTree )
    {
        JPanel kPanel = new JPanel(new GridLayout(1,0));

        //Create the nodes.
        DefaultMutableTreeNode kTop =
            new DefaultMutableTreeNode("StringTree");

        kStringTree.CreateNodes(kTop);

        m_kTree = new JTree(kTop);

        //Listen for when the selection changes.
        m_kTree.addTreeSelectionListener(this);

        //Create the scroll pane and add the tree to it. 
        JScrollPane treeView = new JScrollPane(m_kTree);
        Dimension minimumSize = new Dimension(100, 100);
        treeView.setMinimumSize(minimumSize);

        //Add the pane to the panel.
        kPanel.add(treeView);
        kPanel.setOpaque(true);
        setContentPane(kPanel);

        //Display this window:
        pack();
        setVisible(true);

        kPanel.setSize( 300, 400 );
    }

    /** Required by TreeSelectionListener interface.
     * @param e, the TreeSelectionEvent
     */
    public void valueChanged(TreeSelectionEvent e) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode)
            m_kTree.getLastSelectedPathComponent();

        if (node == null) return;

        //Object nodeInfo = node.getUserObject();
        //if (node.isLeaf()) {
        //} else {
        //}
    }

    /** JTree represnts the scene-graph object. */
    private JTree m_kTree = null;
}
