package gov.nih.mipav.view;

import java.awt.event.KeyEvent;

public class CustomToolBarBuilder extends ViewToolBarBuilder {

	
	public static final ButtonParams PARAM_VOI_DEFAULT_POINTER = new ButtonParams("Pointer", "Default Mode", "pointer");
	
	/** VOI Types*/
	public static final ButtonParams PARAM_VOI_TEXT = new ButtonParams("TextVOI", "Annotation tool", "text");
	public static final ButtonParams PARAM_VOI_POINT = new ButtonParams("Point",
            "<html>" + "Draw point VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "pointROI");
	public static final ButtonParams PARAM_VOI_POLY_SLICE = new ButtonParams("Polyslice", "Draw inter-slice polyline", "polyframe");
	public static final ButtonParams PARAM_VOI_LINE = new ButtonParams("Line", "Draw line VOI", "linear");
	public static final ButtonParams PARAM_VOI_PROTRACTOR = new ButtonParams("protractor", "Protractor tool", "protractor");
	public static final ButtonParams PARAM_VOI_RECTANGLE = new ButtonParams("RectVOI",
            "<html>" + "Draw rectangle VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "rect");
	public static final ButtonParams PARAM_VOI_ELLIPSE = new ButtonParams("EllipseVOI",
            "<html>" + "Draw ellipse VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "circle");
	public static final ButtonParams PARAM_VOI_POLYGON = new ButtonParams("Polyline", "Draw polygon/polyline VOI", "polygon");
	public static final ButtonParams PARAM_VOI_POLYLINE = new ButtonParams("LevelSetVOI",
            "<html>" + "Draw levelset VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "contour");
	public static final ButtonParams PARAM_VOI_LEVELSET = new ButtonParams("LevelSetVOI",
            "<html>" + "Draw levelset VOI" + "<br>" + "Hold SHIFT for multiple drawing" +
            "</html>", "contour");
	public static final ButtonParams PARAM_VOI_LIVEWIRE = new ButtonParams("LiveWireVOI", KeyEvent.VK_L, "Live wire VOI", "livewire");
	public static final ButtonParams PARAM_VOI_3D_RECTANGLE = new ButtonParams("Rect3DVOI", "3D rectangular VOI", "cube");
	public static final ButtonParams PARAM_VOI_SPLITTER = new ButtonParams("SplitVOI", "Split VOI Contour", "split");
	
	/** VOI Actions/Properties */
	public static final ButtonParams PARAM_VOI_NEW = new ButtonParams("NewVOI", "Initiate new VOI", "newvoi");
	public static final ButtonParams PARAM_VOI_COLOR = new ButtonParams("Pointer", "Default Mode", "pointer");
	public static final ButtonParams PARAM_VOI_UNDO = new ButtonParams("undoVOI", "Undo last VOI change (Ctrl-Z)", "undopaint");
	public static final ButtonParams PARAM_VOI_CUT = new ButtonParams("cutVOI", "Cut selected contour (Ctrl-X)", "cutpaint");
	public static final ButtonParams PARAM_VOI_COPY = new ButtonParams("copyVOI", "Copy selected contour (Ctrl-C)", "copypaint");
	public static final ButtonParams PARAM_VOI_PASTE = new ButtonParams("pasteVOI", "Paste contour (Ctrl-V)", "pastepaint");
	public static final ButtonParams PARAM_VOI_PROPAGATE_DOWN = new ButtonParams("PropVOIDown", "Propagate VOI down", "voipropd");
	public static final ButtonParams PARAM_VOI_PROPAGATE_UP = new ButtonParams("PropVOIUp", "Propagate VOI up", "voipropu");
	public static final ButtonParams PARAM_VOI_PROPAGATE_ALL = new ButtonParams("PropVOIAll", "Propagate VOI to all slices", "voipropall");
	public static final ButtonParams PARAM_VOI_QUICK_AND_OP = new ButtonParams("QuickMask", "<html>" + "Quick AND VOI mask operation." +
            "<br>" + "[right-click for options]" + "</html>", "quickvoimask");
	public static final ButtonParams PARAM_VOI_QUICK_OR_OP = new ButtonParams("QuickMaskReverse", "<html>" + "Quick NOT VOI mask operation." +
            "<br>" + "[right-click for options]" + "</html>", "quickvoimaskreverse");
	
	
	/** Paint Mask toolbar buttons */
	public static final ButtonParams PARAM_PAINT_ADD_MASK = new ButtonParams("NewMask", "Add a blank mask.", "newmask");
	public static final ButtonParams PARAM_PAINT_OPEN_MASK = new ButtonParams("OpenMask", "Open mask from a file.", "openmask");
	public static final ButtonParams PARAM_PAINT_SAVE_MASK = new ButtonParams("SaveMask", "Save current mask.", "savemask");
	public static final ButtonParams PARAM_PAINT_AND_OP_MASK = new ButtonParams("AndMask", "AND mask operation.", "andmask");
	
	/** Painting buttons*/
	public static final ButtonParams PARAM_PAINT_BRUSH = new ButtonParams("PaintBrush", "Draw using a brush.", "brush");
	public static final ButtonParams PARAM_PAINT_ADVANCED = new ButtonParams("AdvancedPaint", "Load advanced paint tools", "advancedpaint");
	public static final ButtonParams PARAM_PAINT_DROPPER = new ButtonParams("Dropper", "Picks up a color from the image.", "dropper");
	public static final ButtonParams PARAM_PAINT_FILL = new ButtonParams("PaintCan", "Fills an area with desired color.", "paintcan");
	public static final ButtonParams PARAM_PAINT_ERASER = new ButtonParams("Eraser", "Erases paint.", "eraser");
	public static final ButtonParams PARAM_PAINT_ERASE_SLICE = new ButtonParams("EraseCurrent", "Erase paint from current frame", "clearcurrent");
	public static final ButtonParams PARAM_PAINT_ERASE_ALL = new ButtonParams("EraseAll", "Erase all paint.", "clear");
	
	/** Paint propagate actions */
	public static final ButtonParams PARAM_PAINT_PROPAGATE_DOWN = new ButtonParams("PropagatePaintPrev", "Propagate the paint to the previous slice", "paintpropd");
	public static final ButtonParams PARAM_PAINT_PROPAGATE_UP = new ButtonParams("PropagatePaintNext", "Propagate the paint to the next slice", "paintpropu");
	public static final ButtonParams PARAM_PAINT_PROPAGATE_ALL = new ButtonParams("PropagatePaintAll", "Propagate the paint to all slices", "paintpropall");
	
	/** Misc Paint actions*/
	public static final ButtonParams PARAM_PAINT_BRUSH_EDITOR = new ButtonParams("PaintBrushEditor", "Paint brush editor.", "paint_brush_editor");
	public static final ButtonParams PARAM_PAINT_COLOR = new ButtonParams("colorPaint", "Change paint color.", "colorpaint");
	public static final ButtonParams PARAM_PAINT_RGB_CHOOSER = new ButtonParams("RGBPaintComponent", "Choose RGB components of applied paint.", "rgbcomp");
	public static final ButtonParams PARAM_PAINT_OPACITY = new ButtonParams("OpacityPaint", "Change opacity of paint.", "opacity");
	public static final ButtonParams PARAM_PAINT_BORDER = new ButtonParams("DisplayBorder", "Display/Hide border around painted areas.", "borderpaint");
	
	/** Paint mask ops */
	public static final ButtonParams PARAM_PAINT_MASK_INSIDE = new ButtonParams("CommitPaint", "<html>" + "Masks the inside of the painted area." +
            "<br>" + "[right-click for options]" + "</html>", "paintinside");
	public static final ButtonParams PARAM_PAINT_MASK_OUTSIDE = new ButtonParams("CommitPaintExt", "<html>" + "Masks the outside of the painted area." +
            "<br>" + "[right-click for options]" + "</html>", "paintoutside");
	
	public static final ButtonParams PARAM_PAINT_UNDO = new ButtonParams("UndoPaint", "Undo last paint/region grow.", "undopaint");
	public static final ButtonParams PARAM_PAINT_VOLUME_CALCULATOR = new ButtonParams("CalcPaint", "Calculate volume of painted regions.", "calc");
	public static final ButtonParams PARAM_PAINT_POWERPAINT = new ButtonParams("PowerPaint", "Load power paint tools", "powerpaint");
	
	/** Script buttons */
	public static final ButtonParams PARAM_SCRIPT_REFRESH = new ButtonParams("ToolbarScriptRefresh", "Refresh script listing.", "refresh");
	public static final ButtonParams PARAM_SCRIPT_RUN = new ButtonParams("ToolbarScriptRun", "Run the selected script.", "play");
	public static final ButtonParams PARAM_SCRIPT_RECORD = new ButtonParams("PowerPaint", "Load power paint tools", "powerpaint");
	
	/** Image toolbar Buttons */
	public static final ButtonParams PARAM_IMAGE_OPEN = new ButtonParams("OpenNewImage", "Open image", "open");
	public static final ButtonParams PARAM_IMAGE_SAVE = new ButtonParams("SaveImage", "Save image", "save");
	public static final ButtonParams PARAM_IMAGE_PRINT = new ButtonParams("PrintImage", "Print image", "printer");
	public static final ButtonParams PARAM_IMAGE_CAPTURE = new ButtonParams("CaptureTiff", "Capture image to TIFF(RGB)", "camera");
	public static final ButtonParams PARAM_IMAGE_HEADER = new ButtonParams("AboutImage", "View Header", "header");
	public static final ButtonParams PARAM_IMAGE_ATTRIBUTES = new ButtonParams("EditImageInfo", "Edit attributes", "attributes");
	
	/** LUT buttons*/
	public static final ButtonParams PARAM_LUT = new ButtonParams("DisplayLUT", "Displays Lookup Table (LUT)", "histolut");
	public static final ButtonParams PARAM_CT = new ButtonParams("ctPresetsLUT", "CT preset function", "ctwindow");
	public static final ButtonParams PARAM_WINDOW_LEVEL = new ButtonParams("winLevel", "Adjust window and level", "winlevel");
	public static final ButtonParams PARAM_LUT_QUICK = new ButtonParams("quickLUT", "Quick LUT", "quicklut");
	public static final ButtonParams PARAM_LUT_RESET = new ButtonParams("resetLUTs", "Reset LUT", "resetlut");
	public static final ButtonParams PARAM_LUT_INVERT = new ButtonParams("invertLUT", "Invert LUT", "invert");
	
	/** LUT Color buttons */
	public static final ButtonParams PARAM_LUT_GRAY = new ButtonParams("GrayLUT", "Gray LUT", "gray");
	public static final ButtonParams PARAM_LUT_RED = new ButtonParams("redLUT", "Red LUT", "redlut");
	public static final ButtonParams PARAM_LUT_GREEN = new ButtonParams("greenLUT", "Green LUT", "greenlut");
	public static final ButtonParams PARAM_LUT_BLUE = new ButtonParams("blueLUT", "Blue LUT", "bluelut");
	public static final ButtonParams PARAM_LUT_GRAY_BLUE_RED = new ButtonParams("graybrLUT", "Gray Blue/Red LUT", "graybr");
	public static final ButtonParams PARAM_LUT_HOTMETAL = new ButtonParams("HotMetalLUT", "Hot Metal LUT", "hotmetal");
	public static final ButtonParams PARAM_LUT_SPECTRUM = new ButtonParams("spectrumLUT", "Spectrum LUT", "spectrum");
	public static final ButtonParams PARAM_LUT_COOL_HOT = new ButtonParams("coolHotLUT", "Cool hot LUT", "coolhot");
	public static final ButtonParams PARAM_LUT_SKIN = new ButtonParams("skinLUT", "Skin LUT", "skin");
	public static final ButtonParams PARAM_LUT_BONE = new ButtonParams("boneLUT", "Bone LUT", "bone");
	public static final ButtonParams PARAM_LUT_STRIPED = new ButtonParams("stripedLUT", "Striped LUT", "stripedLUT");
	
	/** LUT transfer/thresholding */
	public static final ButtonParams PARAM_LUT_TRANSFER = new ButtonParams("linearLUT", "Transfer function", "transfer");
	public static final ButtonParams PARAM_LUT_TRANSFER_RESET = new ButtonParams("resetLinearLUT", "Reset transfer function", "linear");
	public static final ButtonParams PARAM_LUT_TRANSFER_EVEN_DIST = new ButtonParams("evendistriLUT", "Even distribution function", "evendistri");
	public static final ButtonParams PARAM_LUT_THRESHOLD = new ButtonParams("thresholdLUT", "Dual threshold function", "threshold");
	public static final ButtonParams PARAM_LUT_THRESHOLD_INVERSE = new ButtonParams("inverseThresholdLUT", "Dual inverse threshold function", "thresholdinverse");
	
	public static final ButtonParams PARAM_LUT_CT_PRESETS = new ButtonParams("ctPresetsLUT", "CT preset function", "ctwindow");
	
	/** LUT channel edit*/
	public static final ButtonParams PARAM_LUT_EDIT_ALPHA = new ButtonParams("alpha", "Edit alpha function", "alpha");
	public static final ButtonParams PARAM_LUT_EDIT_RED = new ButtonParams("red", "Edit red LUT function", "red");
	public static final ButtonParams PARAM_LUT_EDIT_GREEN = new ButtonParams("green", "Edit green LUT function", "green");
	public static final ButtonParams PARAM_LUT_EDIT_BLUE = new ButtonParams("blue", "Edit blue LUT function", "blue");
	public static final ButtonParams PARAM_LUT_GENERATE = new ButtonParams("GenerateLUT", "Generate LUT table", "luttable");
	
	/** LUT Save/open buttons*/
	public static final ButtonParams PARAM_LUT_OPEN = new ButtonParams("OpenUDLUT", "Open user defined LUT", "userlutopen");
	public static final ButtonParams PARAM_LUT_SAVE = new ButtonParams("PowerPaint", "Load power paint tools", "powerpaint");
	
	/** Image slider/frame related buttons */	
	public static final ButtonParams PARAM_IMAGE_SLICE_PREVIOUS = new ButtonParams("PreviousImage",
            "<html>" + "Decrements image slice" + "<br>" +
            "Hold SHIFT to sync other images" + "</html>", "leftarrow");
	public static final ButtonParams PARAM_IMAGE_SLICE_NEXT = new ButtonParams("NextImage",
            "<html>" + "Increments image slice" + "<br>" +
            "Hold SHIFT to sync other images" + "</html>", "rightarrow");
	public static final ButtonParams PARAM_IMAGE_LINK = new ButtonParams("ScrollLink", 
			"Link images of like-dimensions for scrolling. (or hold <SHIFT> while scrolling) ", "link_broken.gif");
	
	/** Image Magnifying related buttons */
	public static final ButtonParams PARAM_IMAGE_MAG = new ButtonParams("MagImage",
            "<html>" + "Magnify image 2.0x" + "<br>" + "Hold SHIFT for multiple zooming" +
            "<br>" + "[right-click for options]" + "</html>", "zoomin");
	public static final ButtonParams PARAM_IMAGE_UNMAG = new ButtonParams("UnMagImage",
            "<html>" + "Magnify image 0.5x" + "<br>" + "Hold SHIFT for multiple zooming" +
            "<br>" + "[right-click for options]" + "</html>", "zoomout");
	public static final ButtonParams PARAM_IMAGE_MAG_REGION = new ButtonParams("MagRegion", "Magnify Region", "magregion");
	public static final ButtonParams PARAM_IMAGE_MAG_WINDOW = new ButtonParams("WinRegion", "Window region of image B", "winregion");
	public static final ButtonParams PARAM_IMAGE_MAG_CHECKER = new ButtonParams("CheckerBoard", "Checker Board A&B", "checker");
	public static final ButtonParams PARAM_IMAGE_MAG_ONE_TO_ONE = new ButtonParams("ZoomOne", "Magnify image 1.0x", "zoom1");
	
	/** Image alternate views (triplanar/volume renderer) related buttons */
	public static final ButtonParams PARAM_IMAGE_TRIPLANAR = new ButtonParams("Tri-planar", "Tri-Planar View", "3plane");
	public static final ButtonParams PARAM_IMAGE_VOLUME_RENDERER = new ButtonParams("VolTriplanar", "Volume Renderer", "java3d");
	public static final ButtonParams PARAM_IMAGE_VOLUME_RENDERER_GPU = new ButtonParams("WMVolTriplanar", "GPU-based Volume Renderer - v1.0", "wm");
	public static final ButtonParams PARAM_IMAGE_VOLUME_RENDERER_DTI = new ButtonParams("DTIStandAlone", "DTI Volume Renderer Standalone - Beta", "vr");
	public static final ButtonParams PARAM_IMAGE_LIGHTBOX = new ButtonParams("Light box", "View Light Box", "lightbox");
	
	/** Image flipping buttons */
	public static final ButtonParams PARAM_IMAGE_FLIP_HORIZONTAL = new ButtonParams("ImageFlipY", "Flip horizontally", "fliphoriz");
	public static final ButtonParams PARAM_IMAGE_FLIP_VERTICAL = new ButtonParams("ImageFlipX", "Flip vertically", "flipvert");
	
	public CustomToolBarBuilder(Object _UI) {
		super(_UI);
	}
	
	
	public static class ButtonParams {
		public static int INVALID_MNEMONIC = -1;
		String cmd, tooltip, iconbase;
		int mnemonic = INVALID_MNEMONIC;
		public ButtonParams(String cmd, String tooltip, String iconbase) {
			this.cmd = cmd;
			this.tooltip = tooltip;
			this.iconbase = iconbase;
		}
		
		public ButtonParams(String cmd, int mnemonic, String tooltip, String iconbase) {
			this(cmd,tooltip,iconbase);
			this.mnemonic = mnemonic;
		}
	}
}
