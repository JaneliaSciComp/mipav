package gov.nih.mipav.plugins;


public class BundledPlugInInfo {
    public String category;

    public String name;

    public BundledPlugInInfo(final String cat, final String pluginName) {
        category = cat;
        name = pluginName;
    }

    public String getCategory() {
        return category;
    }

    public String getName() {
        return name;
    }

    public void setCategory(final String cat) {
        category = cat;
    }

    public void setName(final String pluginName) {
        name = pluginName;
    }
}
