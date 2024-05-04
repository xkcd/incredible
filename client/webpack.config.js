import ReactRefreshWebpackPlugin from '@pmmmwh/react-refresh-webpack-plugin'
import HtmlWebpackPlugin from 'html-webpack-plugin'
import path from 'path'
import reactRefreshBabel from 'react-refresh/babel'
import TerserPlugin from 'terser-webpack-plugin'
import webpack from 'webpack'

import comicData from './comic.json' assert { type: 'json' }

function buildComic(_env, argv) {
  return {
    name: 'comic',
    entry: {
      index: './src/index.tsx',
      moderator: './src/page/moderator.tsx',
      'demo-editor': './src/page/demo-editor.tsx',
      'demo-map': './src/page/demo-map.tsx',
      'demo-viewer': './src/page/demo-viewer.tsx',
    },
    output: {
      path: path.resolve(import.meta.dirname, 'built'),
      filename: '[name].js',
      publicPath: comicData.publicPath,
    },
    module: {
      rules: [
        {
          test: /\.tsx?$/,
          use: {
            loader: 'babel-loader',
            options: {
              plugins: [
                argv.mode === 'development' && reactRefreshBabel,
              ].filter(Boolean),
            },
          },
          exclude: /node_modules/,
        },
        {
          test: /\.png$/,
          use: {
            loader: 'comic-image-loader',
            options: {
              name: 'static/[contenthash:6].[ext]',
              publicPath: comicData.publicPath,
              quant: true,
              baseScale: 4,
              scales: [2, 4],
            },
          },
        },
      ],
    },
    resolve: {
      extensions: ['.tsx', '.ts', '.js'],
      alias: {
        lodash: 'lodash-es',
        '@art': path.resolve(import.meta.dirname, 'art/'),
      },
    },
    resolveLoader: {
      modules: ['node_modules', path.resolve(import.meta.dirname, 'loaders')],
    },
    cache: argv.mode === 'development' ? { type: 'filesystem' } : false,
    devtool: argv.mode === 'development' ? 'eval-source-map' : 'source-map',
    optimization: {
      minimizer: [
        new TerserPlugin({
          extractComments: false,
        }),
      ],
    },
    plugins: [
      new webpack.BannerPlugin(
        'by chromako.de, spyhi, oh no, LiraNuna, and DirtyPunk',
      ),
      new webpack.EnvironmentPlugin({ API_ENDPOINT: null }),
      argv.mode === 'development' && new ReactRefreshWebpackPlugin(),
      ...['index', 'moderator', 'demo-map', 'demo-editor', 'demo-viewer'].map(
        (name) =>
          new HtmlWebpackPlugin({
            inject: false,
            minify: false,
            scriptLoading: 'blocking',
            template: `src/${name === 'index' ? 'index' : 'page/page'}.ejs`,
            filename: `${name}.html`,
            chunks: [name],
            templateParameters: (_compilation, _assets, assetTags) => ({
              tags: assetTags,
              comic: comicData,
            }),
          }),
      ),
    ].filter(Boolean),
    experiments: {
      asyncWebAssembly: true,
    },
    devServer: {
      hot: true,
    },
  }
}

export default buildComic
