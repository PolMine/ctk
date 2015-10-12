import java.io.FileReader;
import java.io.IOException;
import java.util.List;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.HasWord;
import edu.stanford.nlp.international.french.process.FrenchTokenizer;
import edu.stanford.nlp.process.*;


public class MyFrenchTokenizer {

  public static void main(String[] args) throws IOException {
    for (String arg : args) {

      TokenizerFactory<CoreLabel> tokenizerFactory = FrenchTokenizer.ftbFactory();
      FileReader r = new FileReader(arg);
      edu.stanford.nlp.process.Tokenizer<CoreLabel> tokenizer = tokenizerFactory.getTokenizer(r);

      while (tokenizer.hasNext()) {
         CoreLabel label = tokenizer.next();
         System.out.println(label);
      }

    }
  }
}
