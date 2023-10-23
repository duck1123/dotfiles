{
  programs.ncmpcpp = {
    enable = false;
    settings = {
      # Playlist
      now_playing_prefix = "$b";
      now_playing_suffix = "$/b";
      playlist_display_mode = "columns (classic/columns)";
      autocenter_mode = "yes";
      centered_cursor = "yes";
      visualizer_fifo_path = "/tmp/mpd.fifo";
      visualizer_output_name = "my_fifo";
      visualizer_sync_interval = "30";
      visualizer_in_stereo = "yes";
      # visualizer_type = "wave" (spectrum/wave)
      visualizer_type = "spectrum (spectrum/wave)";
      visualizer_color = "green";
      visualizer_look = "∙▋";
      # progressbar_look = "━■"
      progressbar_look = "▀▀ ";
      progressbar_color = "black";
      progressbar_elapsed_color = "blue";
      # Bars
      song_list_format = " $1%a $5//$8 %t";
      # song_list_format = "$1• $8%t $1by $2%a$2 $R$1%l";
      # song_list_format = "$8 %t $R$8";
      song_status_format = " $2%a $4⟫$3⟫ $8%t $4⟫$3⟫ $5%b ";
      # song_status_format = "%t » %a »{ %b » }%y";
      # progressbar_look = "|] ";
      titles_visibility = "no";
      song_columns_list_format = "(6f)[default]{l} (40)[default]{t|f} (25)[default]{a} (30)[default]{b}";
      # Browser
      browser_playlist_prefix = "$2plist »$9 ";
      browser_display_mode = "columns (classic/columns)";
      mouse_support = "yes";
      header_visibility = "no";
      statusbar_visibility = "yes";
      enable_window_title = "no";
      # Colors
      discard_colors_if_item_is_selected = "yes";
      header_window_color = "default";
      volume_color = "default";
      state_line_color = "default";
      state_flags_color = "default";
      main_window_color = "default";
      color1 = "default";
      color2 = "default";
      main_window_highlight_color = "white";
      statusbar_color = "default";
      active_column_color = "default";
      # Others
      song_window_title_format = "{%a - }{%t}{ - %b{ Disc %d}}|{%f}";
      search_engine_display_mode = "columns (classic/columns)";
      follow_now_playing_lyrics = "yes";
      display_screens_numbers_on_start = "no";
      clock_display_seconds = "yes";
      # execute_on_song_change = "twmnc -c \"$(ncmpcpp --now-playing %a' - '%t)\";
    };
  };

}
