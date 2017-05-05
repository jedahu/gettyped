// TODO https://stackoverflow.com/questions/34851839/how-to-handle-web-workers-standard-syntax-with-webpack
const webpack = require("webpack");
module.exports = {
    entry: {
        app: "./app/main/index.ts",
        worker: "./app/worker/index.ts"
    },
    output: {
        filename: "[name].bundle.js",
        chunkFilename: "[id].chunk.js",
        path: __dirname + "/site"
    },

    devtool: "source-map",

    resolve: {
        extensions: [".ts", ".tsx", ".js", ".json"]
    },

    module: {
        rules: [
            {
                test: /app\/main\/.*?\.tsx?$/,
                loader: "awesome-typescript-loader",
                options: {
                    configFileName: "./app/main/tsconfig.json"
                }
            },
            {
                test: /app\/worker\/.*?\.tsx?$/,
                loader: "awesome-typescript-loader",
                options: {
                    configFileName: "./app/worker/tsconfig.json"
                }
            },
            {
                enforce: "pre",
                test: /\.js$/,
                loader: "source-map-loader"
            },
            {
                test: /\.txt$/,
                loader: "raw-loader"
            }
        ]
    },

    externals: {
        "react": "React",
        "react-dom": "ReactDOM",
        "ace": "ace"
    },

    node: {
        fs: "empty"
    },

    devServer: {
        contentBase: "./site"
    }
};
