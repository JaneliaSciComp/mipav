package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to indicate type of image when the program doesn't recognize the name.
 *
 * @version  1.0 Feb 4, 1999
 * @author   Matthew McAuliffe, Ph.D.
 * @see      FileIO
 */
public class JDialogUnknownIO extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 960085709232971480L;

    /** DOCUMENT ME! */
    private static String[] typeNames = new String[] {
                                            "AFNI (.brik, .head)", "Analyze (.img)", "AVI (.avi)", "Biorad (.pic)",
                                            "BMP (.bmp)", "Bruker (.brk)", "Cheshire (.imc)", "COR (.info)",
                                            "DICOM (.dcm, .ima)", "DM3 (.dm3)", "FITS (.fits)", "GE Genesis (.sig)",
                                            "GE Signa4x (.gedno)",
                                            "GIF (.gif)", "ICS (.ics, .ids)", "Interfile (.hdr)", "JPEG (.jpg, .jpeg)",
                                            "LSM (.lsm)", "Magnetom Vision (.ima)", "Map (.map)",

                                            // "Medvision (.bin)",
                                            "Micro Cat (.log)", "Micro Cat raw (.ct)", "Minc (.mnc)", "MRC (.mrc)",
                                            "NIFTI(.img)", "NIFTI (.nii)", "NRRD (.nhdr)", "NRRD(.nrrd)",
                                            "OSM (.wu)", "PCX (.pcx)", "PICT (.pict)",
                                            "PNG (.png)", "PSD (.psd)", "Quicktime (.qt, .mov)", "Raw (.raw)",
                                            "SPM (.spm)", "STK (.stk)", "TGA (.tga)", "TIFF (.tif)", "TIFF (.tiff)",
                                            "VOI (.voi)", "XBM (.xbm)", "XML (.xml)", "XPM (.xpm)"
                                        };

    /** DOCUMENT ME! */
    private static String[] typeSuffices = new String[] {
                                               ".brik", ".img", ".avi", ".pic", ".bmp", ".brk", ".imc", ".info", ".dcm",
                                               ".dm3", ".fits", ".sig", ".gedno", ".gif", ".ics", ".hdr", ".jpg",
                                               ".lsm", ".ima", ".map",

                                               // ".bin",
                                               ".log", ".ct", ".mnc", ".mrc", ".img", ".nii",
                                               ".nhdr", ".nrrd", ".wu", ".pcx", ".pict",
                                               ".png", ".psd", ".qt", ".raw", ".spm", ".stk", ".tga", ".tif", ".tiff",
                                               ".voi", ".xbm", ".xml", ".xpm"
                                           };

    /** DOCUMENT ME! */
    private static int[] typeInts = new int[] {
                                        FileBase.AFNI, FileBase.ANALYZE, FileBase.AVI, FileBase.BIORAD, FileBase.JIMI,
                                        FileBase.BRUKER, FileBase.CHESHIRE, FileBase.COR, FileBase.DICOM, FileBase.DM3,
                                        FileBase.FITS, FileBase.GE_GENESIS, FileBase.GE_SIGNA4X, FileBase.JIMI,

                                        // FileBase.DICOM,
                                        FileBase.ICS, FileBase.INTERFILE, FileBase.JIMI, FileBase.LSM,
                                        FileBase.MAGNETOM_VISION, FileBase.MAP, FileBase.MICRO_CAT, FileBase.MICRO_CAT,
                                        FileBase.MINC, FileBase.MRC, FileBase.NIFTI, FileBase.NIFTI, 
                                        FileBase.NRRD, FileBase.NRRD, FileBase.OSM,
                                        FileBase.JIMI, FileBase.JIMI, FileBase.JIMI, FileBase.JIMI, FileBase.QT,
                                        FileBase.RAW, FileBase.SPM, FileBase.STK, FileBase.JIMI, FileBase.TIFF,
                                        FileBase.TIFF, FileBase.VOI_FILE, FileBase.JIMI, FileBase.XML, FileBase.JIMI
                                    };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int imageType;

    /** DOCUMENT ME! */
    private JList list;

    /** DOCUMENT ME! */
    private String suffix = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates and displays dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  title           Title of dialog frame.
     */
    public JDialogUnknownIO(Frame theParentFrame, String title) {
        super(theParentFrame, true);

        setTitle(title);
        setResizable(true);

        list = new JList(typeNames);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        list.setVisibleRowCount(15);

        JScrollPane sp = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sp);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel OKCancelPanel = new JPanel();
        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        OKCancelPanel.add(OKButton);
        OKCancelPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        pack();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns the suffix, without the ".", based on the index.
     *
     * @param   index  Index into suffix array.
     *
     * @return  Suffix without ".".
     */
    public static String getSuffixFromIndex(int index) {
        String end = typeSuffices[index];

        if (end.length() < 1) {
            return "";
        }

        return end.substring(1);
    }

    /**
     * Accessor that returns the FileBase image type based on the index.
     *
     * @param   index  Index into file type array.
     *
     * @return  FileBase type.
     */
    public static int getTypeFromIndex(int index) {
        return typeInts[index];
    }

    /**
     * Accessor that returns the static list of type names.
     *
     * @return  Array of type names for IO.
     */
    public static String[] getTypeNames() {
        return typeNames;
    }

    /**
     * Closes dialog box when the OK button is pressed and sets the image type and suffix.
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {
            int index = list.getSelectedIndex();
            imageType = typeInts[index];
            suffix = typeSuffices[index];
            dispose();
        } else if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        }
    }

    /**
     * Accessor that returns the image type.
     *
     * @return  The image type.
     */
    public int getImageType() {
        return imageType;
    }

    /**
     * Accessor that returns the suffix.
     *
     * @return  The suffix.
     */
    public String getSuffix() {
        return suffix;
    }
}
