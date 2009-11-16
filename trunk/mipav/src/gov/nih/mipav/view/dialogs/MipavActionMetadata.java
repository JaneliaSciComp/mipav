package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.MipavUtil;


public abstract class MipavActionMetadata implements ActionMetadata {

    public String[] getAuthors() {
        return new String[] {"Matthew J. McAuliffe"};
    }

    public String[] getAffiliation() {
        return new String[] {"National Institutes of Health"};
    }

    public abstract String getCategory();

    public String getShortLabel() {
        return new String();
    }

    public abstract String getLabel();

    public abstract String getName();

    public String getDescription() {
        return new String();
    }

    public String getDescriptionLong() {
        return new String();
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
