
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}

{$IFNDEF NOSTATIC}
  {$IFDEF STD}
    {$IFNDEF LITE}
      {$DEFINE NOSTATIC}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
    {$IFDEF LINUX}
      {$DEFINE LINUX_FPC}
    {$ELSE}
      {$IFDEF DARWIN}
        {$DEFINE DARWIN_FPC}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF NOSTATIC}
    {$IFDEF IOS}
      {$IFNDEF CPUARM}
        {$DEFINE NOSTATIC}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF NOSTATIC}
  {$IFNDEF MSWINDOWS}
    {$IFNDEF FPC}
      {$DEFINE CUSTOM_VFS}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WIN_VFS}
    {$DEFINE CUSTOM_VFS}
  {$ENDIF}

  {$IFDEF CUSTOM_VFS}
    {$IFDEF ANDROID}
      {$DEFINE MOBILE_VFS}
      {$DEFINE ANDROID_VFS}
      {$DEFINE NAMED_EXTERNAL}
      {$DEFINE SET_CODEC}
    {$ELSE}
      {$IFDEF IOS}
        {$IFDEF CPUARM}
          {$DEFINE MOBILE_VFS}
          {$DEFINE IOS_VFS}
          {$DEFINE NAMED_EXTERNAL}
          {$DEFINE SET_CODEC}
        {$ENDIF}
      {$ELSE}
        {$IFDEF MACOS}
          {$DEFINE POSIX_VFS}
          {$DEFINE CUSTOM_FILESTREAM}
          {$IFDEF MACOS64}
            {$DEFINE SET_CODEC}
            {$DEFINE NAMED_EXTERNAL}
          {$ENDIF}
        {$ELSE}
          {$IFDEF POSIX}
            {$DEFINE POSIX_VFS}
            {$DEFINE LINUX_VFS}
            {$DEFINE NAMED_EXTERNAL}
            {$DEFINE SET_CODEC}
            {$DEFINE CUSTOM_FILESTREAM}
          {$ELSE}
            {$IFNDEF WIN_VFS}
              {$DEFINE NOSTATIC}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF LITEDLL}
  {$DEFINE CODEC_CALLBACK}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE CODEC_CALLBACK}
{$ENDIF}

{$IFNDEF NOSTATIC}
  {$DEFINE CODEC}
{$ENDIF}


{$IFDEF NOIMPLEMENT}
  {$UNDEF NOIMPLEMENT}
{$ENDIF}  

{$IFDEF VER20P}
  {$IFDEF MSWINDOWS}
    {$DEFINE NOIMPLEMENT}
  {$ENDIF}
{$ENDIF}
