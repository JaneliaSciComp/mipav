package gov.nih.mipav.view.xcede;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import gov.nih.mipav.model.file.xcede.*;
import gov.nih.mipav.view.*;

public class JXCEDETreeCellRenderer extends DefaultTreeCellRenderer implements TreeCellRenderer {
    protected transient Icon projectOpenIcon;
    protected transient Icon projectClosedIcon;
    protected transient Icon subjectOpenIcon;
    protected transient Icon subjectClosedIcon;
    protected transient Icon visitOpenIcon;
    protected transient Icon visitClosedIcon;
    protected transient Icon studyOpenIcon;
    protected transient Icon studyClosedIcon;
    protected transient Icon seriesOpenIcon;
    protected transient Icon seriesClosedIcon;
    protected transient Icon datarecOpenIcon;
    protected transient Icon datarecClosedIcon;
    protected transient Icon datarecFragOpenIcon;
    protected transient Icon datarecFragClosedIcon;
    
    public JXCEDETreeCellRenderer(){
        super();
        projectOpenIcon = MipavUtil.getIcon("project.gif");
        projectClosedIcon = MipavUtil.getIcon("project_un.gif");
        subjectOpenIcon = MipavUtil.getIcon("subject.gif");
        subjectClosedIcon = MipavUtil.getIcon("subject_un.gif");
        visitOpenIcon = MipavUtil.getIcon("visit.gif");
        visitClosedIcon = MipavUtil.getIcon("visit_un.gif");
        studyOpenIcon = MipavUtil.getIcon("study.gif");
        studyClosedIcon = MipavUtil.getIcon("study_un.gif");
        seriesOpenIcon = MipavUtil.getIcon("series.gif");
        seriesClosedIcon = MipavUtil.getIcon("series_un.gif");
        datarecOpenIcon = MipavUtil.getIcon("datarecord.gif");
        datarecClosedIcon = MipavUtil.getIcon("datarecord_un.gif");
        datarecFragOpenIcon = MipavUtil.getIcon("imagerecord.gif");
        datarecFragClosedIcon = MipavUtil.getIcon("imagerecord_un.gif");
    }
    /**
     * Configures the renderer based on the passed in componenets. The foreground
     * color is set based on the selection and the icon is set based on the leaf
     * and expanded.
     */
    public Component getTreeCellRendererComponent(JTree tree, Object value,
            boolean selected, boolean expanded, boolean leaf, int row,
            boolean hasFocus) {
        super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);
        if(value != null && value instanceof XCEDEElement){
            XCEDEElement element = (XCEDEElement)value;
            if (!tree.isEnabled()) {
                if (element.isProjectLevelElement()) {
                    this.setDisabledIcon(projectClosedIcon);
                } else if (element.isSubjectElement()) {
                    this.setDisabledIcon(subjectClosedIcon);
                } else if (element.isVisitElement()) {
                    this.setDisabledIcon(visitClosedIcon);
                } else if (element.isStudyElement()) {
                    this.setDisabledIcon(studyClosedIcon);
                } else if (element.isSeriesElement()) {
                    this.setDisabledIcon(seriesClosedIcon);
                } else if (element.isDatarecElement()) {
                    this.setDisabledIcon(datarecClosedIcon);
                } else if (element.isDatarecFragElement()) {
                    this.setDisabledIcon(subjectClosedIcon);
                }
            }else{
                if (element.isProjectLevelElement()) {
                    if(expanded){
                        this.setIcon(projectOpenIcon);
                    }else{
                        this.setIcon(projectClosedIcon);
                    }
                } else if (element.isSubjectLevelElement()) {
                    if(expanded){
                        this.setIcon(subjectOpenIcon);
                    }else{
                        this.setIcon(subjectClosedIcon);
                    }
                } else if (element.isVisitLevelElement()) {
                    if(expanded){
                        this.setIcon(visitOpenIcon);
                    }else{
                        this.setIcon(visitClosedIcon);
                    }
                } else if (element.isStudyLevelElement()) {
                    if(expanded){
                        this.setIcon(studyOpenIcon);
                    }else{
                        this.setIcon(studyClosedIcon);
                    }
                } else if (element.isSeriesLevelElement()) {
                    if(expanded){
                        this.setIcon(seriesOpenIcon);
                    }else{
                        this.setIcon(seriesClosedIcon);
                    }
                } else if (element.isDatarecElement()) {
                    if(expanded){
                        this.setIcon(datarecOpenIcon);
                    }else{
                        this.setIcon(datarecClosedIcon);
                    }
                } else if (element.isDatarecFragElement()) {
                    this.setIcon(subjectClosedIcon);
                }
            }
            String id = null;
            if(element.getLevel().equals(XCEDEElement.XCEDE_ELEMENT_DATARECFRAG)){
                id = (String)element.get(XCEDEElement.XCEDE_ELEMENT_FILENAME);
            }else{
                id = (String)element.get(Element.LEVEL);
            }
            if(id == null){
                id = "";
            }
            setText(id);
        }
        return this;
    }
}
