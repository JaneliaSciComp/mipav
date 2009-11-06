package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.MipavUtil;


public abstract class MipavActionMetadata implements ActionMetadata {

    public String getAffiliation() {
        return new String("National Institutes of Health");
    }

    public String getAuthors() {
        return new String("Matthew J. McAuliffe");
    }

    public abstract String getCategory();

    public abstract String getDescription();

    public abstract String getDescriptionLong();

    public abstract String getLabel();

    public abstract String getName();

    public String getPackage() {
        return new String("gov.nih.mipav.view.dialogs.");
    }

    public String getVersion() {
        return MipavUtil.getVersion();
    }

    public String getWebsite() {
        return "http://mipav.cit.nih.gov/";
    }

    public String toString() {
        String str = new String();

        str += "Name:\t\t\t" + getName() + "\n";
        str += "Package:\t\t" + getPackage() + "\n";
        str += "Category:\t\t" + getCategory() + "\n";
        str += "Description:\t\t" + getDescription() + "\n";
        str += "Description Long:\t" + getDescriptionLong() + "\n";
        str += "Label:\t\t\t" + getLabel() + "\n";
        str += "Version:\t\t" + getVersion() + "\n";
        str += "Website:\t\t" + getWebsite() + "\n";
        str += "Authors:\t\t" + getAuthors() + "\n";
        str += "Affiliation:\t\t" + getAffiliation() + "\n";

        return str;
    }
}
