package gov.nih.mipav.view;


import java.awt.*;


/**
 * DOCUMENT ME!
 */
public class ShortCutted {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String alias;

    /** DOCUMENT ME! */
    private Color color;

    /** DOCUMENT ME! */
    private String path;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ShortCutted object.
     *
     * @param  alias  DOCUMENT ME!
     * @param  path   DOCUMENT ME!
     * @param  color  DOCUMENT ME!
     */
    public ShortCutted(String alias, String path, String color) {
        this.alias = alias;
        this.path = path;
        this.color = parseColor(color);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getAlias() {
        return alias;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Color getColor() {
        return color;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getColorString() {
        return colorToString(color);
    }

    /**
     * Formats shortcut's name for display.
     *
     * <p>This method can be modified to meet other display format expectations.</p>
     *
     * @return  DOCUMENT ME!
     */

    public String getDisplayName() {

        if (hasAlias()) {
            return '[' + alias + ']';
        }

        return path;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getName() {

        if (hasAlias()) {
            return alias;
        }

        return path;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPath() {
        return path;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean hasAlias() {
        return (alias.length() > 0);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  newAlias  DOCUMENT ME!
     */
    public void setAlias(String newAlias) {
        alias = newAlias;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  color  DOCUMENT ME!
     */
    public void setColor(String color) {
        this.color = parseColor(color);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String toString() {
        return "[" + alias + "," + path + "," + colorToString(color) + "]";
    }

    /**
     * Converts color to string.
     *
     * <p>Some colors defined in Color are used as is (for instance, Color.blue). Green, teal and yellow colors are
     * defined in this method. Other colors are represented as an RGB hexadecimal string (without the alpha component).
     * </p>
     *
     * @param   color  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    private String colorToString(Color color) {

        if (color == Color.blue) {
            return "blue";
        } else if (color == Color.cyan) {
            return "cyan";
        } else if (color == Color.gray) {
            return "gray";
        } else if (color == Color.magenta) {
            return "magenta";
        } else if (color == Color.orange) {
            return "orange";
        } else if (color == Color.pink) {
            return "pink";
        } else if (color == Color.red) {
            return "red";
        } else if (color == Color.black) {
            return "black";
        }

        String fullColorStr = Integer.toHexString(color.getRGB());

        // The first two digits in fullColorStr are ignored in colorStr (alpha component)
        String colorStr = fullColorStr.substring(2);

        if (colorStr.equals("339933")) {
            return "green";
        } else if (colorStr.equals("cccc33")) {
            return "yellow";
        } else if (colorStr.equals("66cc99")) {
            return "teal";
        }

        return colorStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   colorString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Color parseColor(String colorString) {

        try {
            int rgb = Integer.parseInt(colorString, 16);

            return new Color(rgb);
        } catch (NumberFormatException e) { }

        if (colorString.equals("blue")) {
            return Color.blue;
        }

        if (colorString.equals("cyan")) {
            return Color.cyan;
        }

        if (colorString.equals("gray")) {
            return Color.gray;
        }

        if (colorString.equals("green")) {
            return new Color(0x33, 0x99, 0x33);
        }

        if (colorString.equals("magenta")) {
            return Color.magenta;
        }

        if (colorString.equals("orange")) {
            return Color.orange;
        }

        if (colorString.equals("pink")) {
            return Color.pink;
        }

        if (colorString.equals("red")) {
            return Color.red;
        }

        if (colorString.equals("teal")) {
            return new Color(0x66, 0xcc, 0x99);
        }

        if (colorString.equals("yellow")) {
            return new Color(0xcc, 0xcc, 0x33);
        }

        return Color.black;
    }
}
