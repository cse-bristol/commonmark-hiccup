(ns commonmark-hiccup.core-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [commonmark-hiccup.core :refer :all]))

(deftest markdown->html-test
  (testing "converts a simple string of markdown to html"
    (are [md html] (= html (markdown->html md))
      "Test. Test."              "<p>Test. Test.</p>"
      "# Test\nTesting test."    "<h1>Test</h1><p>Testing test.</p>"
      "## Second Level Heading"  "<h2>Second Level Heading</h2>"
      "### Third Level Heading"  "<h3>Third Level Heading</h3>"
      "#### 4th Level Heading"   "<h4>4th Level Heading</h4>"
      "##### 5th Level Heading"  "<h5>5th Level Heading</h5>"
      "###### 6th Level Heading" "<h6>6th Level Heading</h6>"))
  (testing "replaces line breaks within text contents with spaces by default"
    (is (= (markdown->html "This is a\nparagraph with line\nbreaks.")
           "<p>This is a paragraph with line breaks.</p>")))
  (testing "renders reference links"
    (is (= (markdown->html "This is a [test][1].\n\n[1]: www.test.tst")
           "<p>This is a <a href=\"www.test.tst\">test</a>.</p>")))
  (testing "renders images"
    (are [md html] (= html (markdown->html md))
      "![A pretty picture](./picture.png)" "<p><img alt=\"A pretty picture\" src=\"./picture.png\" /></p>"
      "![A pretty picture](./picture.png \"A Title\")" "<p><img alt=\"A pretty picture\" src=\"./picture.png\" title=\"A Title\" /></p>"
      "![](./picture.png)" "<p><img alt=\"\" src=\"./picture.png\" /></p>"))
  (testing "renders fenced code blocks"
    (is (= (markdown->html "```\n(def foo \"bar\")\n```")
           "<pre><code>(def foo &quot;bar&quot;)\n</code></pre>"))
    (is (= (markdown->html "```clojure\n(def foo \"bar\")\n```")
           "<pre><code class=\"clojure\">(def foo &quot;bar&quot;)\n</code></pre>"))
    (let [config (update-in default-config
                            [:renderer :nodes org.commonmark.node.FencedCodeBlock]
                            (constantly [:pre {:class '("lang:" :node-info " decode:true")} :node-literal]))]
      (is (= (markdown->html config "```clojure\n(def foo \"bar\")\n```")
             "<pre class=\"lang:clojure decode:true\">(def foo &quot;bar&quot;)\n</pre>"))))
  (testing "renders indented code blocks"
    (is (= (markdown->html "    (def foo \"bar\")")
           "<pre><code>(def foo &quot;bar&quot;)\n</code></pre>")))
  (testing "renders code spans"
    (is (= (markdown->html "Avoid throwing `NullPointerException` explicitly!")
           "<p>Avoid throwing <code>NullPointerException</code> explicitly!</p>"))
    (is (= (markdown->html "Inline code, e.g. `<script>alert('Test');</script>`, should be escaped.")
           "<p>Inline code, e.g. <code>&lt;script&gt;alert(&apos;Test&apos;);&lt;/script&gt;</code>, should be escaped.</p>")))
  (testing "renders emphasis and strong emphasis"
    (is (= (markdown->html "This is _emphasized_ text.")
           "<p>This is <em>emphasized</em> text.</p>"))
    (is (= (markdown->html "This is *emphasized* text.")
           "<p>This is <em>emphasized</em> text.</p>"))
    (is (= (markdown->html "This is **strongly emphasized** text.")
           "<p>This is <strong>strongly emphasized</strong> text.</p>")))
  (testing "renders blockquotes"
    (is (= (markdown->html "> This is a\n> blockquote!")
           "<blockquote><p>This is a blockquote!</p></blockquote>")))
  (testing "renders tight lists"
    (is (= (markdown->html "- foo\n- bar\n- baz")
           "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"))
    (is (= (markdown->html "- foo\n  - bar\n  - baz")
           "<ul><li>foo<ul><li>bar</li><li>baz</li></ul></li></ul>"))
    (is (= (markdown->html "1. foo\n2. bar")
           "<ol><li>foo</li><li>bar</li></ol>"))
    (is (= (markdown->html "1. foo\n2. bar\n3) baz")
           "<ol><li>foo</li><li>bar</li></ol><ol start=\"3\"><li>baz</li></ol>")))
  (testing "renders loose lists"
    (is (= (markdown->html "- foo\n\n- bar\n\n- baz")
           "<ul><li><p>foo</p></li><li><p>bar</p></li><li><p>baz</p></li></ul>")))
  (testing "renders HTML blocks"
    (is (= (markdown->html "<script>alert('Test');</script>")
           "<script>alert('Test');</script>")))
  (testing "renders inline HTML"
    (is (= (markdown->html "This is <foo>text</bar> surrounded by tags.")
           "<p>This is <foo>text</bar> surrounded by tags.</p>")))
  (testing "renders thematic breaks"
    (is (= (markdown->html "# a\n---")
           "<h1>a</h1><hr />")))
  (testing "renders hard line breaks"
    (is (= (markdown->html "This is a\\\nline with\\\nhard line breaks.")
           "<p>This is a<br />line with<br />hard line breaks.</p>"))))

(deftest commonmark-extensions-test
  (testing "renders tables"
    (is (= (markdown->html "|head1|head2|\n|---|---|\n|foo|bar|")
           "<table><thead><tr><td>head1</td><td>head2</td></tr></thead><tbody><tr><td>foo</td><td>bar</td></tr></tbody></table>"))))

(deftest anchor-links
  (testing "nodes can be given valid IDs to be used as link targets"
    (let [config (assoc-in default-config
                           [:renderer :nodes org.commonmark.node.Heading]
                           ['(:h :node-level) {:id :anchor} :content])]
      (is (= "<h2 id=\"abc_def\">abc def</h2>"
             (markdown->html config "## abc def")))
      
      (is (= "<h2 id=\"abc_def\">abc <strong>def</strong></h2>"
             (markdown->html config "## abc **def**"))))))

(deftest yaml-front-matter
  (testing "Handles yaml front matter"
    (is (= {"key" ["value"], "k2" ["v2"]}
           (:front-matter (markdown->hiccup-with-front-matter "---\nkey: value\nk2: v2\n---\n\n## head\nabc\n\ndef"))))
           
    (is (= '([:h2 ("head")] [:p ("abc")] [:p ("def")])
           (:hiccup (markdown->hiccup-with-front-matter "---\nkey: value\nk2: v2\n---\n\n## head\nabc\n\ndef"))
           (:hiccup (markdown->hiccup-with-front-matter "## head\nabc\n\ndef"))
           (markdown->hiccup "## head\nabc\n\ndef")
           (markdown->hiccup "---\nkey: value\nk2: v2\n---\n\n## head\nabc\n\ndef")))
    ))
