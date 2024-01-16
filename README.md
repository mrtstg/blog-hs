# blog-hs

[Прочитать на русском](./README.ru.md)

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
![YAML](https://img.shields.io/badge/yaml-%23ffffff.svg?style=for-the-badge&logo=yaml&logoColor=151515)
![Redis](https://img.shields.io/badge/redis-%23DD0031.svg?style=for-the-badge&logo=redis&logoColor=white)
![SQLite](https://img.shields.io/badge/sqlite-%2307405e.svg?style=for-the-badge&logo=sqlite&logoColor=white)

A lightweight blog platform, written in Haskell using [Yesod](https://www.yesodweb.com/). 
Using [Redis](https://redis.io/) for caching data and sqlite for providing sorted data.

**This branch does not includes any CSS for web pages. If you're looking for already designed site, check `main-bulma` branch.**

# Features
- Fully SSR! :rocket:
- Posts stored in MD files
- Sitemap.xml autogeneration
- Internal Redis caching
- [OpenGraph](https://ogp.me) and [JSON-LD](https://json-ld.org/) generation for pages

# Table of contents

- [How it works](#how-it-works)
- [Usage](#usage)
- [Configuration](#configuration)
- [Deploy](#deploy)
- [License](#lisense)

# How it works

The main idea of project is storing posts of blog in Markdown files in `templates` directory, so the name
of files and folders will be URL of page. Each post consists of two files:
- Markdown file, which includes page content
- YML file, which inclues information about post like title, image, description etc.

For example, post `https://YOURSITE.HERE/post/haskell/how-to-start-yesod` will be stored in files 
`./templates/haskell/how-to-start-yesod.md` and `./templates/haskell/how-to-start-yesod.yml`.

Parsing of markdown file written in [Attoparsec](https://hackage.haskell.org/package/attoparsec) in [`Parser` module](src/Parser/)
and theoretically can be extended. Now, its implemented basic elements of Markdown like headers, lists, code blocks, quotes,
images, links, bold and italic texts.

Result of parsing both files is stored in Redis for one minute. This timing can be changed in [Redis module](src/App/Redis.hs).

Also, blog-hs stores data about posts in SQLite database, which is used for showing latest posts on index page and [generating
sitemap.xml](src/Handlers/Sitemap.hs) file. Database is creating manually using `create-db` command and stores data from YML files.

# Usage

## Flags

- `-f <PATH>`: sets path for [config file](#configuration)

- `-p <PORT>`: sets port for server to listen. Default is 3000.

## Commands

Blog-hs currently have several commands:

- check: validates existence of YML files for every MD file in `./templates/` directory (note that its relative path and depends
on your working directory).

![image](photos/check1.png)

- create-db: creates (or replaces) SQLite database. Also validates files first.

![image](photos/createdb1.png)

- run: launches the server.

# Configuration

Configuration file can be passed using [`-f` flag](#flags). In other case, server will take important parameters from environment
variables or will use default values.

| Field in config file | Environment variable name | Usage | Default value |
|----------------------|---------------------------|-------|---------------|
| redisHost | REDIS_HOST | Address for Redis host | localhost |
| redisPort | REDIS_PORT | Port of redis server | 6379 |
| dbPath | DB_PATH | Path of SQLite database | ./blog.db |
| blogDepthLimit | DEPTH_LEVEL | How deep is allowed to go into folders. If request will try to get file deeper than limit, server will response PermissionDenied error. | 1 |
| enableIndexPage | ENABLE_INDEX | Enable index page. If disabled, server will response not found error. | True (1) |
| siteName | SITE_NAME | Short description of your website. Used in OG and JSON-LD | - |
| siteHost | SITE_HOST | Base host of site. For example, "https://mrtstg.ru". No trailing slash at end. Used in generating links. | - |
| robotsFilePath | ROBOTS_TXT_PATH | Path for robots.txt file. | - |

# Deploy

Instead of launching built binary there is way for launch server in Docker container using Docker Compose. 
If you have `make`, you make it using:

```bash
make build-image
make deploy
```

For destroying containers, use `make destroy` command.

This method by default suggests that you will configure server using environment variables. Add volume for config file
if its nececcary.

If you want only to build image, use `make build-image` command.

:exclamation: **Dont forget to correct robots.txt file! It's not getting generated automatically.**

# License

Code is under BSD 3 Clause license. Please have a look at the [LICENSE.md](./LICENSE) for details.
