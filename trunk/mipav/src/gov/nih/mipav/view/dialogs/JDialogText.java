package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * This is simple text dialog that displays in the center of the screen. It automatically adjusts the dialog size to the
 * length of the input string.
 *
 * @version  0.1 Oct 1, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 *
 *           <p>Defaults to an interface very similar to the original version, however now supports plain text, HTML and
 *           Rich-Text format. Perhaps the most significant difference is that when text is too long to fit entirely
 *           within the width of the display-area, it is wrapped around to the other side, rather than the scroll-area
 *           increasing in size.</p>
 *
 *           <p>Plain text format is handled by the <a
 *           href="http://java.sun.com/j2se/1.4.1/docs/api/javax/swing/text/DefaultEditorKit.html">DefaultEditorKit</a>.
 *           The HTML formatting is handled by <a
 *           href="http://java.sun.com/j2se/1.4.1/docs/api/javax/swing/text/html/HTMLEditorKit.html">the HTML editor
 *           kit</a>. HTMLEditorKit parses HTML 3.2, so be sure to see <a href="http://www.w3.org/TR/REC-html32">the
 *           World Wide Web Consortium's (w3) definitive reference for HTML 3.2</a>. The RTF parsing is handled by the
 *           <a href="http://java.sun.com/j2se/1.4.1/docs/api/javax/swing/text/rtf/RTFEditorKit.html">RTF Editor
 *           Kit</a>, which appears to only be a bare-bones implementation.</p>
 *
 *           <p>By default, all text sent to this editor (use the method <code>append(String)</code> or <code>
 *           setText(String)</code>) is formatted as plain-text, with <code>PLAIN_FORMAT</code>. The user-code can
 *           reformat using <code>setFormat(String)</code>, and either <code>PLAIN_FORMAT</code> <code>
 *           HTML_FORMAT</code>, or <code>RTF_FORMAT</code> as any predefined setting, or any normal MIME type. Refer to
 *           <a href="http://www.ietf.org/rfc/1rfc_index.txt">the RFC list</a> or <a
 *           href="http://www.ietf.org/rfc/rfc2045.txt">RFC 2045 (MIME Types, part 1)</a> and <a
 *           href="http://www.ietf.org/rfc/rfc2045.txt">RFC 2045 (MIME Types part 2)</a> for more on MIME types.</p>
 * @version  2.0 2003-April-9
 */
public class JDialogText extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3861014709972568409L;

    /**
     * Specifis the output display of the text-box to be formatted using basic HTML rules, as defined by MIME-format
     * &quot;text/html&quot;.
     *
     * <p>For more information, consult <a href="http://www.ietf.org/rfc/rfc1866.txt">RFC 1866 (HTML 2.0)</a>, section
     * 4.1, which defines the MIME type with all valid options; since RFC 1866 is out-dated, see also <a
     * href="http://www.ietf.org/rfc/rfc2854.txt">RFC 2854</a>, a more more up-to-date source on &quot;text/html&quot;
     * practices. RFC 2854, however, details HTML 4.0, which carries tags not deciphered by the decoder here; for
     * information on HTML 3.2 (and the tags which may be used in included texts), see <a
     * href="http://www.w3.org/TR/REC-html32">World Wide Web Consortium's (w3) definitive reference for HTML 3.2</a>.
     * </p>
     */
    public static final String HTML_FORMAT = "text/html";

    /** Specifies the output display of the text-box to be formatted using the RTF rules. */
    public static final String RTF_FORMAT = "text/rtf";

    /**
     * Specifies that the textbox not format the text in the display, as is defined by MIME type &quot;text/plain&quot;.
     * In addition, this setting will also reset the font to serif10.
     */
    public static final String PLAIN_FORMAT = "text/plain";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Describes the initial size of the textual display area when the dialog is created. The value is given in pixel
     * size rather than the number of characters since the display area has no characters to display.
     */
    protected final Dimension DEFAULT_SIZE = new Dimension(650, 400);


    /** DOCUMENT ME! */
    private JPanel buttonPanel;

    /** DOCUMENT ME! */
    private boolean followingScroll = false;

    /** DOCUMENT ME! */
    private StringBuffer message;

    /** DOCUMENT ME! */
    private JScrollPane scrollPane; // here so we can set scroll correct

    /** DOCUMENT ME! */
    private JEditorPane textArea;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs resizable dialog with text area in the middle.
     *
     * @param  parent  Parent frame.
     * @param  title   Title of dialog frame.
     */
    public JDialogText(Frame parent, String title) {
        super(parent, false);
        setResizable(true);
        message = new StringBuffer("");
        init(title);
    }

    /**
     * Constructs a resizable dialog with a text area which can be filled; the scrollbar can be set to keep up with the
     * updates (at the bottom).
     *
     * @param  parent        parent frame
     * @param  title         title of the dialog frame
     * @param  followScroll  <code>true</code> means that the scroll bar should stay at the bottom of the text area
     *                       where appended messages would go. <code>false</code> will not dynamically update the scroll
     *                       bar location.
     */
    public JDialogText(Frame parent, String title, boolean followScroll) {
        super(parent, false);
        message = new StringBuffer("");
        followingScroll = followScroll;

        if (followScroll) {
            setResizable(true);
            init(title);
            scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
        } else {
            setResizable(true);
            init(title);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the "Close" button is pressed.
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == cancelButton) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Appends the text area with the message.
     *
     * @param  appMessage  The message to append to the text area.
     */
    public void append(String appMessage) {
        message.append(appMessage);
        textArea.setText(message.toString());

        if (!followingScroll) {
            setScrollPaneTop();
        }
    }

    /**
     * Uses the input string as the text to display in the dialog's text area, formats it with the specified MIME-type
     * (or with <code>PLAIN_FORMAT</code> if it can't determine the how to display the content).
     *
     * @param  type     Either a MIME-type or <code>PLAIN_FORMAT</code>, <code>RTF_FORMAT</code>, or <code>
     *                  HTML_FORMAT</code>.
     * @param  content  The String for the dialog to display in its text area.
     */
    public void setContent(String type, String content) {
        setMessage(content); // so that the 'content' is used in next call!
        setFormat(type);
    }

    /**
     * Attempts to read the given file, place its contents into the dialog's display area and format it using the
     * specified MIME-type (or with <code>PLAIN_FORMAT</code> if it can not determine how to display the content).
     *
     * @param   type     Either a MIME-type or <code>PLAIN_FORMAT</code>, <code>RTF_FORMAT</code>, or <code>
     *                   HTML_FORMAT</code>.
     * @param   content  The File whose contents the dialog is to display in its text area.
     *
     * @throws  FileNotFoundException  DOCUMENT ME!
     * @throws  IOException            DOCUMENT ME!
     */
    public void setContent(String type, File content) throws FileNotFoundException, IOException {
        StringBuffer newMessage = new StringBuffer();
        FileReader fr = new FileReader(content);
        BufferedReader buffReader = new BufferedReader(fr);

        String s = buffReader.readLine();

        while (s != null) {
            newMessage.append(s + "\n");
            s = buffReader.readLine();
        }

        setContent(type, newMessage.toString());
        setScrollPaneTop();
    }

    /**
     * Attempts to set the size of the text display area to the supplied number of text-columns and rows.
     *
     * <p>Negative or zero values are ignored, and the current value for text display area size is used in that
     * dimension.</p>
     *
     * <p>The font size is assumed to be the size of a character of <code>MipavUtil.font12</code>, a fixed-width font.
     * Obviously, this means that content-types displaying HTML may be larger (or smaller) so although the size will be
     * set, it will not have much meaning for these texts.</p>
     *
     * @param  columns  The number of text-columns of <code>MipavUtil.font12</code> width.
     * @param  rows     The number of text-columns of <code>MipavUtil.font12</code> height.
     */
    public void setDocumentSize(int columns, int rows) {
        int w = getFontMetrics(MipavUtil.font12).getMaxAdvance() * columns;
        int h = getFontMetrics(MipavUtil.font12).getHeight() * rows;

        if (w <= 0) {
            w = textArea.getWidth();
        }

        if (h <= 0) {
            h = textArea.getHeight();
        }

        textArea.setSize(w, h);
        textArea.setMinimumSize(textArea.getSize());
    }

    /**
     * Sets the editor kit that renders the dialog's text area to the type given.
     *
     * <p>A <code>PLAIN_FORMAT</code> or content-type that is unrecognized (and subsequently set to <code>
     * PLAIN_FORMAT</code>) will be set to use a fixed-width font. The content is then re-loaded.</p>
     *
     * @param  type  Either a MIME-type or <code>PLAIN_FORMAT</code>, <code>RTF_FORMAT</code>, or <code>
     *               HTML_FORMAT</code>.
     */
    public void setFormat(String type) {
        textArea.setContentType(type); // if type incorrect, will set to plain

        if (type.equals(PLAIN_FORMAT) || textArea.getContentType().equals(PLAIN_FORMAT)) {
            textArea.setFont(MipavUtil.courier12);
        }

        textArea.setText(message.toString()); // reload the text content
    }

    /**
     * Sets the text area to the message, erasing what was there earlier.
     *
     * @param  message  Message to display in the text area.
     */
    public void setMessage(String message) {
        this.message.delete(0, this.message.length());
        this.message.append(message);
        textArea.setText(message);

        if (!followingScroll) {
            setScrollPaneTop(); // FIXME: does not function as-expected
        }
    }

    /**
     * Sets the caret to the top of the scroll pane.
     */
    public void setScrollPaneTop() {
        textArea.setCaretPosition(0);
    }

    /**
     * Accessor to the <code>JPanel</code> which holds the buttons at the bottom of the dialog.
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel getButtonPanel() {
        return buttonPanel;
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     *
     * @param  title  Title of the dialog box.
     */
    protected void init(String title) {
        JPanel scrollPanel;

        setTitle(title);

        textArea = new JEditorPane();
        setFormat(PLAIN_FORMAT);
        textArea.setBackground(Color.white);
        textArea.setEditable(false);

        scrollPane = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPanel = new JPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(scrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(10, 15, 10, 15));

        buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(cancelButton);
        getContentPane().add(scrollPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setSize(DEFAULT_SIZE);
    }


}
