// TODO https://stackoverflow.com/questions/34851839/how-to-handle-web-workers-standard-syntax-with-webpack
const webpack = require("webpack");
const ExtractTextPlugin = require("extract-text-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");
const ScssVars = require("./scss/vars");

const DEV = process.env.NODE_ENV === "development";

const extractBundles = (bs : Array<any>) =>
       bs.map(b => new webpack.optimize.CommonsChunkPlugin(b));

const extractCss = new ExtractTextPlugin("styles.css");
const extractSass = new ExtractTextPlugin({
    filename: "[name].[contenthash].css",
    disable: false //DEV
});

module.exports = {
    entry: {
        app: [
            "./app/main/run.tsx"
        ]
    },
    output: {
        filename: "[name].bundle.[hash].js",
        chunkFilename: "[id].chunk.[hash].js",
        path: __dirname + "/site"
    },

    devtool: false,

    resolve: {
        extensions: [".ts", ".tsx", ".js", ".json"]
    },

    module: {
        rules: [
            {
                test: /\.tsx?$/,
                loader: "awesome-typescript-loader",
                options: {
                    configFileName: "./app/main/tsconfig.json",
                    transpileOnly: true,
                    useCache: true
                }
            },
            {
                test: /\.(txt|md)$/,
                loader: "raw-loader"
            },
            {
                test: /\.css$/,
                use: extractCss.extract(["css-loader"])
            },
            {
                test: /\.scss$/,
                use: extractSass.extract({
                    use: [{
                        loader: "css-loader"
                    }, {
                        loader: "sass-loader",
                        options: {
                            data: ScssVars.asString()
                        }
                   }],
                   fallback: "style-loader"
               })
           },
           {
               test: /\.eot|ttf|woff$/,
               use: "file-loader?name=fonts/[name].[ext]"
           }
       ]
   },

   plugins: [
       new webpack.DllReferencePlugin({
           context: ".",
           manifest: require("./site/lib-vendor-manifest.json")
       }),
       extractCss,
       extractSass,
       new FaviconsWebpackPlugin("./logo.png"),
       new HtmlWebpackPlugin({
           title: "Get Typed",
           inject: "head",
           hash: true,
           cache: true,
           showErrors: false,
           xhtml: false,
           filename: "index.html",
           template: "index.ejs"
       }),
       new CopyWebpackPlugin([{
           from: `node_modules/monaco-editor/${DEV ? "dev" : "min"}/vs`,
           to: "vs"
       }])
   ],

   externals: {
   },

   node: {
       fs: "empty"
   },

   devServer: {
       contentBase: "./site",
       historyApiFallback: true,
       disableHostCheck: true,
       host: "0.0.0.0" /*,
       allowedHosts: [ "210.54.32.44" ]*/
   }
}
