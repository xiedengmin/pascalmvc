-- phpMyAdmin SQL Dump
-- version 5.0.2
-- https://www.phpmyadmin.net/
--
-- 主机： localhost
-- 生成日期： 2022-05-19 15:33:48
-- 服务器版本： 5.7.27-log
-- PHP 版本： 7.3.8

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- 数据库： `pascalmvc`
--

-- --------------------------------------------------------

--
-- 表的结构 `article`
--

CREATE TABLE `article` (
  `ID` bigint(20) NOT NULL,
  `CreatedAt` bigint(20) DEFAULT NULL,
  `ModifiedAt` bigint(20) DEFAULT NULL,
  `Title` varchar(120) DEFAULT NULL,
  `Content` mediumtext,
  `ContentHtml` int(11) DEFAULT NULL,
  `Author` bigint(20) DEFAULT NULL,
  `AuthorName` varchar(50) DEFAULT NULL,
  `PublishedMonth` bigint(20) DEFAULT NULL,
  `abstract` mediumtext,
  `Tags` mediumblob
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `author`
--

CREATE TABLE `author` (
  `ID` bigint(20) NOT NULL,
  `CreatedAt` bigint(20) DEFAULT NULL,
  `ModifiedAt` bigint(20) DEFAULT NULL,
  `LogonName` varchar(30) NOT NULL,
  `FirstName` varchar(50) DEFAULT NULL,
  `FamilyName` varchar(50) DEFAULT NULL,
  `BirthDate` datetime DEFAULT NULL,
  `Email` varchar(40) DEFAULT NULL,
  `HashedPassword` varchar(64) DEFAULT NULL,
  `Verified` int(11) DEFAULT NULL,
  `Rights` bigint(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- 转存表中的数据 `author`
--

INSERT INTO `author` (`ID`, `CreatedAt`, `ModifiedAt`, `LogonName`, `FirstName`, `FamilyName`, `BirthDate`, `Email`, `HashedPassword`, `Verified`, `Rights`) VALUES
(1, 135712663775, 135712663775, 'synopse', '', 'Synopse', '1899-12-30 00:00:00', '', 'aa4aecd02efeaab94433ff1eae7479a43bcfa5e685e8c97f7c321ffe57d23daf', 1, 15);

-- --------------------------------------------------------

--
-- 表的结构 `bloginfo`
--

CREATE TABLE `bloginfo` (
  `ID` bigint(20) NOT NULL,
  `Title` varchar(80) DEFAULT NULL,
  `Language_` varchar(3) DEFAULT NULL,
  `Description` varchar(120) DEFAULT NULL,
  `Copyright` varchar(80) DEFAULT NULL,
  `About` mediumtext
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `comment`
--

CREATE TABLE `comment` (
  `ID` bigint(20) NOT NULL,
  `CreatedAt` bigint(20) DEFAULT NULL,
  `ModifiedAt` bigint(20) DEFAULT NULL,
  `Title` varchar(120) DEFAULT NULL,
  `Content` mediumtext,
  `ContentHtml` int(11) DEFAULT NULL,
  `Author` bigint(20) DEFAULT NULL,
  `AuthorName` varchar(50) DEFAULT NULL,
  `Article` bigint(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `dict_menu`
--

CREATE TABLE `dict_menu` (
  `id` int(11) DEFAULT NULL,
  `menuname` char(50) DEFAULT NULL,
  `pid` int(11) DEFAULT NULL,
  `url` char(200) DEFAULT NULL,
  `s_id` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- 转存表中的数据 `dict_menu`
--

INSERT INTO `dict_menu` (`id`, `menuname`, `pid`, `url`, `s_id`) VALUES
(1, '会员管理', 1, '/vip/', 3),
(2, '充值记录', 1, '/pay/', 4),
(3, '权限管理', 1, '/Role/', 2),
(4, '用户管理', 1, '/User/', 1),
(5, 'JWT演示', 1, '/jwt/', 5),
(6, '上传图片', 1, '/upimage/', 6);

-- --------------------------------------------------------

--
-- 表的结构 `dict_role`
--

CREATE TABLE `dict_role` (
  `id` int(11) DEFAULT NULL,
  `rolename` char(50) DEFAULT NULL,
  `menus` char(50) DEFAULT NULL,
  `powers` char(50) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- 转存表中的数据 `dict_role`
--

INSERT INTO `dict_role` (`id`, `rolename`, `menus`, `powers`) VALUES
(1, '管理员', '3,1,2,4,', '1'),
(2, '测试员', '3,1,2,4,', '2');

-- --------------------------------------------------------

--
-- 表的结构 `tag`
--

CREATE TABLE `tag` (
  `ID` bigint(20) NOT NULL,
  `Ident` varchar(80) DEFAULT NULL,
  `Occurence` bigint(20) DEFAULT NULL,
  `CreatedAt` bigint(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- 表的结构 `users`
--

CREATE TABLE `users` (
  `id` int(11) NOT NULL,
  `username` char(20) DEFAULT NULL,
  `pwd` char(20) DEFAULT NULL,
  `sex` char(4) DEFAULT NULL,
  `address` char(200) DEFAULT NULL,
  `age` int(11) DEFAULT NULL,
  `idcard` char(20) DEFAULT NULL,
  `realname` char(50) DEFAULT NULL,
  `phone` char(20) DEFAULT NULL,
  `roleid` int(11) DEFAULT NULL,
  `uptime` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- 转存表中的数据 `users`
--

INSERT INTO `users` (`id`, `username`, `pwd`, `sex`, `address`, `age`, `idcard`, `realname`, `phone`, `roleid`, `uptime`) VALUES
(2, 'admin', 'admin', '0', 'xxx', 10, '2', '刘启4', '2', 1, '2022-05-07 16:02:47'),
(3, 'test44', '123', NULL, NULL, NULL, NULL, '张大帅', NULL, 2, '2022-04-12 20:23:03'),
(4, 'admin7', 'xdm4', NULL, NULL, NULL, NULL, '赵丽颖', NULL, 2, '2022-03-14 18:38:13'),
(5, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(6, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(7, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(8, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(9, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(10, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(11, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(12, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00'),
(13, 'coolmvc', '0003456', '', '', 0, '', '', '18950868703', 0, '1899-12-30 00:00:00');

--
-- 转储表的索引
--

--
-- 表的索引 `article`
--
ALTER TABLE `article`
  ADD PRIMARY KEY (`ID`),
  ADD KEY `NDXarticleAuthor` (`Author`),
  ADD KEY `NDXarticlePublishedMonth` (`PublishedMonth`);

--
-- 表的索引 `author`
--
ALTER TABLE `author`
  ADD PRIMARY KEY (`ID`),
  ADD UNIQUE KEY `LogonName` (`LogonName`);

--
-- 表的索引 `bloginfo`
--
ALTER TABLE `bloginfo`
  ADD PRIMARY KEY (`ID`);

--
-- 表的索引 `comment`
--
ALTER TABLE `comment`
  ADD PRIMARY KEY (`ID`),
  ADD KEY `NDXcommentAuthor` (`Author`),
  ADD KEY `NDXcommentArticle` (`Article`);

--
-- 表的索引 `tag`
--
ALTER TABLE `tag`
  ADD PRIMARY KEY (`ID`);

--
-- 表的索引 `users`
--
ALTER TABLE `users`
  ADD PRIMARY KEY (`id`);

--
-- 在导出的表使用AUTO_INCREMENT
--

--
-- 使用表AUTO_INCREMENT `users`
--
ALTER TABLE `users`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=14;
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
