-- phpMyAdmin SQL Dump
-- version 5.1.1
-- https://www.phpmyadmin.net/
--
-- 主机： localhost
-- 生成日期： 2022-05-06 21:10:38
-- 服务器版本： 5.7.26
-- PHP 版本： 7.3.4

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
(2, 'admin', 'admin', '0', 'xxx', 10, '2', '刘启3', '2', 1, '2022-05-05 09:57:06'),
(3, 'test44', '123', NULL, NULL, NULL, NULL, '张大帅', NULL, 2, '2022-04-12 20:23:03'),
(4, 'admin7', '123', NULL, NULL, NULL, NULL, '赵丽颖', NULL, 2, '2022-03-14 18:38:13');

--
-- 转储表的索引
--

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
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=5;
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
