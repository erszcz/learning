import java.io.File;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RE {

    public static void main (String[] args) throws Exception {
        String post = new Scanner(new File("post.txt"))
            .useDelimiter("\\Z").next();

        Pattern p = Pattern.compile("(<!--QuoteBegin(.*?)-->(.*?)<!--QuoteEEnd-->)|" +
                                    "(<div\\b[^>]*class=['\"](signature|quotetop|quotemain)['\"][^>]*>(.*?)</div>)",
                                    Pattern.MULTILINE | Pattern.DOTALL);
        Matcher m = p.matcher(post);
        System.out.println(m.replaceAll(""));
    }

}
