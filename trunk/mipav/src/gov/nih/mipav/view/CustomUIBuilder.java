package gov.nih.mipav.view;

import java.awt.event.KeyEvent;

public class CustomUIBuilder {

	
	public static final UIParams PARAM_VOI_DEFAULT_POINTER = new UIParams("Default Pointer", "Pointer", UIParams.INVALID_MNEMONIC, 
			"Default Mode", "pointer", true);
	
	/** VOI Types*/
	public static final UIParams PARAM_VOI_TEXT = new UIParams("Draw Text VOI", "TextVOI", UIParams.INVALID_MNEMONIC, 
			"Annotation tool", "text", true);
	public static final UIParams PARAM_VOI_POINT = new UIParams("Draw Point VOI", "Point", UIParams.INVALID_MNEMONIC,
            "<html>" + "Draw point VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "pointROI", true);
	public static final UIParams PARAM_VOI_POLY_SLICE = new UIParams("Draw Poly-slice VOI","Polyslice", UIParams.INVALID_MNEMONIC, "Draw inter-slice polyline", "polyframe", true);
	public static final UIParams PARAM_VOI_LINE = new UIParams("Draw Line VOI","Line", UIParams.INVALID_MNEMONIC, "Draw line VOI", "linear", true);
	public static final UIParams PARAM_VOI_PROTRACTOR = new UIParams("Draw Protractor VOI","protractor", UIParams.INVALID_MNEMONIC, "Protractor tool", "protractor", true);
	public static final UIParams PARAM_VOI_RECTANGLE = new UIParams("Draw Rectangle VOI","RectVOI", UIParams.INVALID_MNEMONIC,
            "<html>" + "Draw rectangle VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "rect", true);
	public static final UIParams PARAM_VOI_ELLIPSE = new UIParams("Draw Ellipse VOI","EllipseVOI", UIParams.INVALID_MNEMONIC,
            "<html>" + "Draw ellipse VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "circle", true);
	public static final UIParams PARAM_VOI_POLYGON = new UIParams("Draw Polygon VOI","Polyline", UIParams.INVALID_MNEMONIC, 
			"Draw polygon/polyline VOI", "polygon", true);
	public static final UIParams PARAM_VOI_POLYLINE = new UIParams("Draw Polyline VOI","Polyline", UIParams.INVALID_MNEMONIC,
            "<html>" + "Draw levelset VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "contour", true);
	public static final UIParams PARAM_VOI_LEVELSET = new UIParams("Draw LevelSet VOI","LevelSetVOI", UIParams.INVALID_MNEMONIC,
            "<html>" + "Draw levelset VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "contour", true);
	public static final UIParams PARAM_VOI_LIVEWIRE = new UIParams("Draw LiveWire VOI","LiveWireVOI", KeyEvent.VK_L, 
			"Live wire VOI", "livewire", true);
	public static final UIParams PARAM_VOI_3D_RECTANGLE = new UIParams("Draw 3D Rectangle VOI", "Rect3DVOI", UIParams.INVALID_MNEMONIC, 
			"3D rectangular VOI", "cube", true);
	public static final UIParams PARAM_VOI_SPLITTER = new UIParams("Split VOI Contour","SplitVOI", UIParams.INVALID_MNEMONIC, 
			"Split VOI Contour", "split", true);
	public static final UIParams PARAM_VOI_POINT_DELETE = new UIParams("Delete point VOI", "deleteVOI", UIParams.INVALID_MNEMONIC, 
			"Delete point VOI", "delete", false);
	public static final UIParams PARAM_VOI_POINT_ADD = new UIParams("Add point", "addPoint", UIParams.INVALID_MNEMONIC, "Add point", "pointROI", false);
	
	/** VOI Actions/Properties */
	public static final UIParams PARAM_VOI_PROPERTIES = new UIParams("VOIProperties", "View VOI Properties", "voiproperties");
	public static final UIParams PARAM_VOI_COLOR = new UIParams("VOIColor", "Current/change VOI Color", "transparent");
	public static final UIParams PARAM_VOI_NEW = new UIParams("NewVOI", "Initiate new VOI", "newvoi");
	public static final UIParams PARAM_VOI_UNDO = new UIParams("undoVOI", "Undo last VOI change (Ctrl-Z)", "undopaint");
	public static final UIParams PARAM_VOI_CUT = new UIParams("cutVOI", "Cut selected contour (Ctrl-X)", "cutpaint");
	public static final UIParams PARAM_VOI_COPY = new UIParams("copyVOI", "Copy selected contour (Ctrl-C)", "copypaint");
	public static final UIParams PARAM_VOI_PASTE = new UIParams("pasteVOI", "Paste contour (Ctrl-V)", "pastepaint");
	public static final UIParams PARAM_VOI_PROPAGATE_DOWN = new UIParams("PropVOIDown", "Propagate VOI down", "voipropd");
	public static final UIParams PARAM_VOI_PROPAGATE_UP = new UIParams("PropVOIUp", "Propagate VOI up", "voipropu");
	public static final UIParams PARAM_VOI_PROPAGATE_ALL = new UIParams("PropVOIAll", "Propagate VOI to all slices", "voipropall");
	public static final UIParams PARAM_VOI_QUICK_AND_OP = new UIParams("QuickMask", "<html>" + "Quick AND VOI mask operation." +
            "<br>" + "[right-click for options]" + "</html>", "quickvoimask");
	public static final UIParams PARAM_VOI_QUICK_NOT_OP = new UIParams("QuickMaskReverse", "<html>" + "Quick NOT VOI mask operation." +
            "<br>" + "[right-click for options]" + "</html>", "quickvoimaskreverse");
	
	
	/** Paint Mask toolbar buttons */
	public static final UIParams PARAM_PAINT_ADD_MASK = new UIParams("NewMask", "Add a blank mask.", "newmask");
	public static final UIParams PARAM_PAINT_OPEN_MASK = new UIParams("OpenMask", "Open mask from a file.", "openmask");
	public static final UIParams PARAM_PAINT_SAVE_MASK = new UIParams("SaveMask", "Save current mask.", "savemask");
	public static final UIParams PARAM_PAINT_AND_OP_MASK = new UIParams("AndMask", "AND mask operation.", "andmask");
	
	/** Painting buttons*/
	public static final UIParams PARAM_PAINT_BRUSH = new UIParams("PaintBrush", "Draw using a brush.", "brush");
	public static final UIParams PARAM_PAINT_ADVANCED = new UIParams("AdvancedPaint", "Load advanced paint tools", "advancedpaint");
	public static final UIParams PARAM_PAINT_DROPPER = new UIParams("Dropper", "Picks up a color from the image.", "dropper");
	public static final UIParams PARAM_PAINT_FILL = new UIParams("PaintCan", "Fills an area with desired color.", "paintcan");
	public static final UIParams PARAM_PAINT_ERASER = new UIParams("Eraser", "Erases paint.", "eraser");
	public static final UIParams PARAM_PAINT_ERASE_SLICE = new UIParams("EraseCurrent", "Erase paint from current frame", "clearcurrent");
	public static final UIParams PARAM_PAINT_ERASE_ALL = new UIParams("EraseAll", "Erase all paint.", "clear");
	
	/** Paint propagate actions */
	public static final UIParams PARAM_PAINT_PROPAGATE_DOWN = new UIParams("PropagatePaintPrev", "Propagate the paint to the previous slice", "paintpropd");
	public static final UIParams PARAM_PAINT_PROPAGATE_UP = new UIParams("PropagatePaintNext", "Propagate the paint to the next slice", "paintpropu");
	public static final UIParams PARAM_PAINT_PROPAGATE_ALL = new UIParams("PropagatePaintAll", "Propagate the paint to all slices", "paintpropall");
	
	/** Misc Paint actions*/
	public static final UIParams PARAM_PAINT_BRUSH_EDITOR = new UIParams("PaintBrushEditor", "Paint brush editor.", "paint_brush_editor");
	public static final UIParams PARAM_PAINT_COLOR = new UIParams("colorPaint", "Change paint color.", "colorpaint");
	public static final UIParams PARAM_PAINT_RGB_CHOOSER = new UIParams("RGBPaintComponent", "Choose RGB components of applied paint.", "rgbcomp");
	public static final UIParams PARAM_PAINT_OPACITY = new UIParams("OpacityPaint", "Change opacity of paint.", "opacity");
	public static final UIParams PARAM_PAINT_BORDER = new UIParams(Preferences.PREF_SHOW_PAINT_BORDER, "Display border around painted areas.",
            "borderpaint");
	
	/** Paint mask ops */
	public static final UIParams PARAM_PAINT_MASK_INSIDE = new UIParams("CommitPaint", "<html>" + "Masks the inside of the painted area." +
            "<br>" + "[right-click for options]" + "</html>", "paintinside");
	public static final UIParams PARAM_PAINT_MASK_OUTSIDE = new UIParams("CommitPaintExt", "<html>" + "Masks the outside of the painted area." +
            "<br>" + "[right-click for options]" + "</html>", "paintoutside");
	
	public static final UIParams PARAM_PAINT_UNDO = new UIParams("UndoPaint", "Undo last paint/region grow.", "undopaint");
	public static final UIParams PARAM_PAINT_VOLUME_CALCULATOR = new UIParams("CalcPaint", "Calculate volume of painted regions.", "calc");
	public static final UIParams PARAM_PAINT_POWERPAINT = new UIParams("PowerPaint", "Load power paint tools", "powerpaint");
	
	/** Script buttons */
	public static final UIParams PARAM_SCRIPT_REFRESH = new UIParams("ToolbarScriptRefresh", "Refresh script listing.", "refresh");
	public static final UIParams PARAM_SCRIPT_RUN = new UIParams("ToolbarScriptRun", "Run the selected script.", "play");
	
	/** Image toolbar Buttons */
	public static final UIParams PARAM_IMAGE_OPEN = new UIParams("OpenNewImage", "Open image", "open");
	public static final UIParams PARAM_IMAGE_SAVE = new UIParams("SaveImage", "Save image", "save");
	public static final UIParams PARAM_IMAGE_PRINT = new UIParams("PrintImage", "Print image", "printer");
	public static final UIParams PARAM_IMAGE_CAPTURE = new UIParams("CaptureTiff", "Capture image to TIFF(RGB)", "camera");
	public static final UIParams PARAM_IMAGE_HEADER = new UIParams("AboutImage", "View Header", "header");
	public static final UIParams PARAM_IMAGE_ATTRIBUTES = new UIParams("EditImageInfo", "Edit attributes", "attributes");
	
	/** LUT buttons*/
	public static final UIParams PARAM_LUT = new UIParams("DisplayLUT", "Displays Lookup Table (LUT)", "histolut");
	public static final UIParams PARAM_LUT_CT = new UIParams("ctPresetsLUT", "CT preset function", "ctwindow");
	public static final UIParams PARAM_WINDOW_LEVEL = new UIParams("winLevel", "Adjust window and level", "winlevel");
	public static final UIParams PARAM_LUT_QUICK = new UIParams("quickLUT", KeyEvent.VK_Q,  "Quick LUT", "quicklut");
	public static final UIParams PARAM_LUT_RESET = new UIParams("resetLUTs", "Reset LUT", "resetlut");
	public static final UIParams PARAM_LUT_INVERT = new UIParams("invertLUT", "Invert LUT", "invert");
	
	/** LUT Color buttons */
	public static final UIParams PARAM_LUT_GRAY = new UIParams("GrayLUT", "Gray LUT", "gray");
	public static final UIParams PARAM_LUT_RED = new UIParams("redLUT", "Red LUT", "redlut");
	public static final UIParams PARAM_LUT_GREEN = new UIParams("greenLUT", "Green LUT", "greenlut");
	public static final UIParams PARAM_LUT_BLUE = new UIParams("blueLUT", "Blue LUT", "bluelut");
	public static final UIParams PARAM_LUT_GRAY_BLUE_RED = new UIParams("graybrLUT", "Gray Blue/Red LUT", "graybr");
	public static final UIParams PARAM_LUT_HOTMETAL = new UIParams("HotMetalLUT", "Hot Metal LUT", "hotmetal");
	public static final UIParams PARAM_LUT_SPECTRUM = new UIParams("spectrumLUT", "Spectrum LUT", "spectrum");
	public static final UIParams PARAM_LUT_COOL_HOT = new UIParams("coolHotLUT", "Cool hot LUT", "coolhot");
	public static final UIParams PARAM_LUT_SKIN = new UIParams("skinLUT", "Skin LUT", "skin");
	public static final UIParams PARAM_LUT_BONE = new UIParams("boneLUT", "Bone LUT", "bone");
	public static final UIParams PARAM_LUT_STRIPED = new UIParams("stripedLUT", "Striped LUT", "stripedLUT");
	
	/** LUT transfer/thresholding */
	public static final UIParams PARAM_LUT_TRANSFER = new UIParams("linearLUT", "Transfer function", "transfer");
	public static final UIParams PARAM_LUT_TRANSFER_RESET = new UIParams("resetLinearLUT", "Reset transfer function", "linear");
	public static final UIParams PARAM_LUT_TRANSFER_EVEN_DIST = new UIParams("evendistriLUT", "Even distribution function", "evendistri");
	public static final UIParams PARAM_LUT_THRESHOLD = new UIParams("thresholdLUT", "Dual threshold function", "threshold");
	public static final UIParams PARAM_LUT_THRESHOLD_INVERSE = new UIParams("inverseThresholdLUT", "Dual inverse threshold function", "thresholdinverse");
	public static final UIParams PARAM_LUT_THRESHOLD_MAX_ENT = new UIParams("maxEntThreshold", "Maximum entropy threshold", "maxent");
	public static final UIParams PARAM_LUT_THRESHOLD_OTSU = new UIParams("otsuThreshold", "Otsu threshold", "otsu");
	public static final UIParams PARAM_LUT_THRESHOLD_RUN = new UIParams("runThreshold", "Run threshold algorithm", "thresholdalgorithm");
	public static final UIParams PARAM_LUT_THRESHOLD_INVERSE_RUN = new UIParams("runInverseThreshold", "Run inverse threshold algorithm",
            "thresholdalgorithminverse");
	
	public static final UIParams PARAM_LUT_CT_PRESETS = new UIParams("ctPresetsLUT", "CT preset function", "ctwindow");
	
	/** LUT channel edit*/
	public static final UIParams PARAM_LUT_EDIT_ALPHA = new UIParams("alpha", "Edit alpha function", "alpha");
	public static final UIParams PARAM_LUT_EDIT_RED = new UIParams("red", "Edit red LUT function", "red");
	public static final UIParams PARAM_LUT_EDIT_GREEN = new UIParams("green", "Edit green LUT function", "green");
	public static final UIParams PARAM_LUT_EDIT_BLUE = new UIParams("blue", "Edit blue LUT function", "blue");
	public static final UIParams PARAM_LUT_EDIT_RGB = new UIParams("all", "Lock RGB functions together", "rgb");
	public static final UIParams PARAM_LUT_GENERATE = new UIParams("GenerateLUT", "Generate LUT table", "luttable");
	
	/** LUT Save/open buttons*/
	public static final UIParams PARAM_LUT_OPEN = new UIParams("OpenUDLUT", "Open user defined LUT", "userlutopen");
	public static final UIParams PARAM_LUT_SAVE = new UIParams("SaveUDLUT", "Save user defined LUT", "userlutsave");
	
	/** Image slider/frame related buttons */	
	public static final UIParams PARAM_IMAGE_SLICE_PREVIOUS = new UIParams("PreviousImage",
            "<html>" + "Decrements image slice" + "<br>" +
            "Hold SHIFT to sync other images" + "</html>", "leftarrow");
	public static final UIParams PARAM_IMAGE_SLICE_NEXT = new UIParams("NextImage",
            "<html>" + "Increments image slice" + "<br>" +
            "Hold SHIFT to sync other images" + "</html>", "rightarrow");
	public static final UIParams PARAM_IMAGE_LINK = new UIParams("ScrollLink", 
			"Link images of like-dimensions for scrolling.", "link_broken.gif");
	
	/** Image Magnifying related buttons */
	public static final UIParams PARAM_IMAGE_MAG = new UIParams("MagImage",
            "<html>" + "Magnify image 2.0x" + "<br>" + "Hold SHIFT for multiple zooming" +
            "<br>" + "[right-click for options]" + "</html>", "zoomin");
	public static final UIParams PARAM_IMAGE_UNMAG = new UIParams("UnMagImage",
            "<html>" + "Magnify image 0.5x" + "<br>" + "Hold SHIFT for multiple zooming" +
            "<br>" + "[right-click for options]" + "</html>", "zoomout");
	public static final UIParams PARAM_IMAGE_MAG_REGION = new UIParams("MagRegion", "Magnify Region", "magregion");
	public static final UIParams PARAM_IMAGE_MAG_WINDOW = new UIParams("WinRegion", "Window region of image B", "winregion");
	public static final UIParams PARAM_IMAGE_MAG_CHECKER = new UIParams("CheckerBoard", "Checker Board A&B", "checker");
	public static final UIParams PARAM_IMAGE_MAG_ONE_TO_ONE = new UIParams("ZoomOne", "Magnify image 1.0x", "zoom1");
	
	/** Tri-planar specific buttons */
	public static final UIParams PARAM_TRIIMAGE_MAG = new UIParams("IndivMagImage",
			"<html>" + "Magnify individual frame 2.0x" + "<br>" +
			"Hold SHIFT for multiple zooming" + "<br>" +
			"[right-click for options]" + "</html>", "trizoomin");
	public static final UIParams PARAM_TRIIMAGE_UNMAG = new UIParams("IndivMinImage",
			"<html>" + "Magnify individual frame 0.5x" + "<br>" +
			"Hold SHIFT for multiple zooming" + "<br>" +
			"[right-click for options]" + "</html>", "trizoomout");
	public static final UIParams PARAM_TRIIMAGE_CENTER = new UIParams("Center", "Identify center of volume", "centerpt");
	public static final UIParams PARAM_TRIIMAGE_TRAVERSE = new UIParams("traverse", "Traverse image", "translate");
	public static final UIParams PARAM_TRIIMAGE_BOUNDING_BOX = new UIParams("boundingBox", "Show/hide crop volume", "boundingcube");
	
	/** image rotation/crop/algorithm fitting buttons */
	public static final UIParams PARAM_TRANSFORMATION_APPLY = new UIParams("Apply", "Applies rotations and translations",
            "createTransformation");
	public static final UIParams PARAM_TRANSFORMATION_LEAST_SQUARES = new UIParams("leastSquares", "Apply least squares alignment",
			"reglsq.gif");
	
	
	/** Image alternate views (triplanar/volume renderer) related buttons */
	public static final UIParams PARAM_IMAGE_TRIPLANAR = new UIParams("Tri-planar", "Tri-Planar View", "3plane");
	public static final UIParams PARAM_IMAGE_VOLUME_RENDERER = new UIParams("VolTriplanar", "Volume Renderer", "java3d");
	public static final UIParams PARAM_IMAGE_VOLUME_RENDERER_GPU = new UIParams("WMVolTriplanar", "GPU-based Volume Renderer - Beta", "wm");
	public static final UIParams PARAM_IMAGE_VOLUME_RENDERER_DTI = new UIParams("DTIStandAlone", "DTI Volume Renderer Standalone - Beta", "vr");
	public static final UIParams PARAM_IMAGE_LIGHTBOX = new UIParams("Light box", "View Light Box", "lightbox");
	public static final UIParams PARAM_IMAGE_GPU = new UIParams("GPU", "GPU rendering", "gpu");
	public static final UIParams PARAM_IMAGE_MULTI= new UIParams("MultiHisto", "Multi-histo rendering", "multihisto");
	
	/** Image flipping buttons */
	public static final UIParams PARAM_IMAGE_FLIP_HORIZONTAL = new UIParams("ImageFlipY", "Flip horizontally", "fliphoriz");
	public static final UIParams PARAM_IMAGE_FLIP_VERTICAL = new UIParams("ImageFlipX", "Flip vertically", "flipvert");
	
	/** Data provenance buttons */
	public static final UIParams PARAM_DATA_PROVENANCE_OPEN = new UIParams("Open", "Open mipav data-provenance file", "open");
	public static final UIParams PARAM_DATA_PROVENANCE_SAVE = new UIParams("Save", "Save mipav data-provenance file", "save");
		
	
	/**
	 * Class that holds the parameters for creating/catching common buttons and menu items
	 * @author linkb
	 *
	 */
	public static class UIParams {
		public static int INVALID_MNEMONIC = -1;
		private String cmd, tooltip, iconbase, text;
		private int mnemonic = INVALID_MNEMONIC;
		private boolean is_toggle = false;
			
		
		public UIParams(String cmd, String tooltip, String iconbase) {
			this(new String(cmd), cmd, INVALID_MNEMONIC, tooltip, iconbase);
		}
		
		public UIParams(String cmd, int mnemonic, String tooltip, String iconbase) {
			this(new String(cmd), cmd, mnemonic, tooltip, iconbase, false);
		}
		
		public UIParams(String text, String cmd, int mnemonic, String tooltip, String iconbase) {
			this(text, cmd, mnemonic, tooltip, iconbase, false);
		}
		
		public UIParams(String text, String cmd, int mnemonic, String tooltip, String iconbase, boolean isToggle) {
			this.text = text;
			this.cmd = cmd;
			this.tooltip = tooltip;
			this.iconbase = iconbase;
			this.mnemonic = mnemonic;
			this.is_toggle = isToggle;
		}
		
		public String getText() {
			return this.text;
		}
		
		public String getActionCommand() {
			return cmd;
		}
		
		public String getToolTip() {
			return tooltip;
		}
		
		public String getIconBase() {
			return iconbase;
		}
		
		public int getMnemonic() {
			return mnemonic;
		}
		
		public boolean isToggle() {
			return this.is_toggle;
		}
	}
}
