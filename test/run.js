try {
    require("source-map-support").install();
} catch(err) {
}
require("../output/out/goog/bootstrap/nodejs.js");
require("../output/tests.js");

goog.require("cats.testrunner");
goog.require("cljs.nodejscli");
