use anyhow::{anyhow, Result};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use markdown::{to_mdast, mdast::{Node, Heading, Text, Code, List as MdList, ListItem as MdListItem}};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text as RText},
    widgets::{
        BarChart, Block, Borders, Cell, Gauge, List, ListItem, ListState, Paragraph, Row, Sparkline, Table,
        Tabs, Wrap,
    },
    Frame, Terminal,
};
use std::{
    ffi::{c_char, c_int, CStr},
    io::{self, stdout, Write},
    time::{Duration, Instant},
};
use syntect::{
    easy::HighlightLines,
    parsing::SyntaxSet,
    highlighting::{Theme, ThemeSet, Style as SynStyle},
};
use rand::Rng;

// Expanded color parsing with more options and hex support
fn parse_color(color: &str) -> Color {
    if color.starts_with('#') {
        if let Ok(rgb) = hex_to_rgb(color) {
            return Color::Rgb(rgb.0, rgb.1, rgb.2);
        }
    }
    match color.to_lowercase().as_str() {
        "red" => Color::Red,
        "green" => Color::Green,
        "blue" => Color::Blue,
        "yellow" => Color::Yellow,
        "magenta" => Color::Magenta,
        "cyan" => Color::Cyan,
        "white" => Color::White,
        "black" => Color::Black,
        "gray" => Color::Gray,
        "lightred" => Color::LightRed,
        "lightgreen" => Color::LightGreen,
        // Add more
        _ => Color::White,
    }
}

fn hex_to_rgb(hex: &str) -> Result<(u8, u8, u8)> {
    let hex = hex.trim_start_matches('#');
    if hex.len() != 6 {
        return Err(anyhow!("Invalid hex color"));
    }
    let r = u8::from_str_radix(&hex[0..2], 16)?;
    let g = u8::from_str_radix(&hex[2..4], 16)?;
    let b = u8::from_str_radix(&hex[4..6], 16)?;
    Ok((r, g, b))
}

// Expanded style parsing, support multiple styles comma-separated
fn parse_style(style_str: &str) -> Modifier {
    let mut modifier = Modifier::empty();
    for s in style_str.split(',') {
        match s.trim().to_lowercase().as_str() {
            "bold" => modifier.insert(Modifier::BOLD),
            "italic" => modifier.insert(Modifier::ITALIC),
            "underline" => modifier.insert(Modifier::UNDERLINED),
            "dim" => modifier.insert(Modifier::DIM),
            "reversed" => modifier.insert(Modifier::REVERSED),
            "blink" => modifier.insert(Modifier::SLOW_BLINK),
            _ => (),
        }
    }
    modifier
}

// Helper to setup and teardown terminal for TUI renders
fn setup_terminal() -> Result<Terminal<CrosstermBackend<io::Stdout>>> {
    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    Terminal::new(backend).map_err(Into::into)
}

fn teardown_terminal(mut terminal: Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
             LeaveAlternateScreen,
             DisableMouseCapture
    )?;
    terminal.show_cursor()?;
    Ok(())
}

// Render text with style
fn render_text(content: &str, color: &str, style_str: &str) -> Result<()> {
    let style = Style::default().fg(parse_color(color)).add_modifier(parse_style(style_str));
    let styled = Span::styled(content, style);
    println!("{}", styled);
    Ok(())
}

// Expanded table with alignments, styles
fn render_table(headers: &str, rows: &str) -> Result<()> {
    let mut terminal = setup_terminal()?;

    let header_cells: Vec<Cell> = headers.split(',').map(|h| Cell::from(h.trim()).style(Style::default().add_modifier(Modifier::BOLD))).collect();
    let header = Row::new(header_cells.clone()).height(1);

    let row_vec: Vec<Row> = rows.split('|').map(|r| {
        let cells: Vec<Cell> = r.split(',').map(|c| Cell::from(c.trim())).collect();
        Row::new(cells).height(1)
    }).collect();

    let widths: Vec<Constraint> = (0..header_cells.len()).map(|_| Constraint::Percentage(100 / header_cells.len() as u16)).collect();

    let table = Table::new(row_vec, widths.as_slice())
    .header(header)
    .block(Block::default().borders(Borders::ALL).title("Table"))
    .style(Style::default().fg(Color::White))
    .column_spacing(1);

    terminal.draw(|f| {
        f.render_widget(table, f.area());
    })?;

    std::thread::sleep(Duration::from_secs(5)); // Display for a bit
    teardown_terminal(terminal)
}

// Expanded progress with colors based on percent, ratio label
fn render_progress(percent: u16, label: &str) -> Result<()> {
    let mut terminal = setup_terminal()?;

    let percent = percent.clamp(0, 100);
    let gauge_color = if percent < 30 { Color::Red } else if percent < 70 { Color::Yellow } else { Color::Green };

    let gauge = Gauge::default()
    .block(Block::default().borders(Borders::ALL).title(label))
    .gauge_style(Style::default().fg(gauge_color).bg(Color::Black))
    .percent(percent)
    .label(format!("{}/100", percent));

    terminal.draw(|f| {
        f.render_widget(gauge, f.area());
    })?;

    std::thread::sleep(Duration::from_secs(3));
    teardown_terminal(terminal)
}

fn get_ansi_fg(color: Color) -> &'static str {
    match color {
        Color::Red => "31",
        Color::Green => "32",
        Color::Blue => "34",
        Color::Yellow => "33",
        Color::Magenta => "35",
        Color::Cyan => "36",
        _ => "37",
    }
}

// Expanded spinner with more frames, colors
fn render_spinner(duration: u64, message: &str) -> Result<()> {
    let start = Instant::now();
    let frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
    let mut rng = rand::thread_rng();
    let colors = [Color::Red, Color::Green, Color::Blue, Color::Yellow, Color::Magenta, Color::Cyan];

    while start.elapsed() < Duration::from_secs(duration) {
        for frame in &frames {
            let color = colors[rng.gen_range(0..colors.len())];
            print!("\r\x1b[{}m{} {}\x1b[0m", get_ansi_fg(color), frame, message);
            io::stdout().flush()?;
            std::thread::sleep(Duration::from_millis(80));
        }
    }
    println!("\rDone!    ");
    Ok(())
}

// Syntax highlighting unchanged, but can add more themes
fn render_syntax(code: &str, lang: &str) -> Result<()> {
    let ss = SyntaxSet::load_defaults_newlines();
    let syntax = ss.find_syntax_by_extension(lang).unwrap_or_else(|| ss.find_syntax_plain_text());
    let ts = ThemeSet::load_defaults();
    let theme: &Theme = &ts.themes["base16-ocean.dark"];
    let mut h = HighlightLines::new(syntax, theme);
    for line in code.lines() {
        let ranges: Vec<(SynStyle, &str)> = h.highlight_line(line, &ss)?;
        for (style, text) in ranges {
            let fg = style.foreground;
            print!("\x1b[38;2;{};{};{}m{}\x1b[0m", fg.r, fg.g, fg.b, text);
        }
        println!();
    }
    Ok(())
}

// Expanded markdown rendering: convert to styled text
fn render_markdown(content: &str) -> Result<()> {
    let ast = to_mdast(content, &markdown::ParseOptions::default()).map_err(|e| anyhow!("Failed to parse markdown: {:?}", e))?;
    let mut lines: Vec<Line> = vec![];

    fn process_node(node: &Node, lines: &mut Vec<Line>, depth: usize) {
        match node {
            Node::Heading(Heading { depth: _h_depth, children, .. }) => {
                let mut spans: Vec<Span> = vec![];
                for child in children {
                    if let Node::Text(Text { value, .. }) = child {
                        spans.push(Span::styled(value.clone(), Style::default().add_modifier(Modifier::BOLD).fg(Color::Cyan)));
                    }
                    // Handle more
                }
                lines.push(Line::from(spans));
            }
            Node::Paragraph(p) => {
                let mut text = String::new();
                for child in &p.children {
                    if let Node::Text(Text { value, .. }) = child {
                        text.push_str(value);
                    }
                }
                lines.push(Line::from(text));
            }
            Node::Code(Code { value, .. }) => {
                for line in value.lines() {
                    lines.push(Line::from(Span::styled(line.to_string(), Style::default().fg(Color::Green))));
                }
            }
            Node::List(MdList { children, .. }) => {
                for item in children {
                    if let Node::ListItem(MdListItem { children: item_children, .. }) = item {
                        let mut item_text = String::new();
                        for child in item_children {
                            if let Node::Paragraph(p) = child {
                                for p_child in &p.children {
                                    if let Node::Text(Text { value, .. }) = p_child {
                                        item_text.push_str(value);
                                    }
                                }
                            }
                        }
                        lines.push(Line::from(format!("- {}", item_text)));
                    }
                }
            }
            // Add more node types: bold, italic, links, etc.
            _ => (),
        }
        if let Some(children) = node.children() {
            for child in children {
                process_node(child, lines, depth + 1);
            }
        }
    }

    process_node(&ast, &mut lines, 0);

    let paragraph = Paragraph::new(RText::from(lines))
    .block(Block::default().borders(Borders::NONE))
    .wrap(Wrap::default());

    let mut terminal = setup_terminal()?;
    terminal.draw(|f| {
        f.render_widget(paragraph, f.area());
    })?;
    std::thread::sleep(Duration::from_secs(5));
    teardown_terminal(terminal)
}

// Expanded list with better selection, mouse support
fn render_list(items: &str, selectable: bool) -> Result<()> {
    let mut terminal = setup_terminal()?;

    let item_strings: Vec<String> = items.split(',').map(|i| i.trim().to_string()).collect();
    let list_items: Vec<ListItem> = item_strings.iter().map(|i| ListItem::new(i.as_str()).style(Style::default().fg(Color::White))).collect();

    let mut state = ListState::default();
    state.select(Some(0));

    let mut selected = None;

    loop {
        terminal.draw(|f| {
            let list = List::new(list_items.clone())
            .block(Block::default().borders(Borders::ALL).title("List"))
            .highlight_style(Style::default().add_modifier(Modifier::BOLD | Modifier::REVERSED).fg(Color::Green))
            .highlight_symbol(">> ");
            f.render_stateful_widget(list, f.area(), &mut state);
        })?;

        if !selectable {
            break;
        }

        if let Event::Key(key) = event::read()? {
            if key.kind == KeyEventKind::Press {
                match key.code {
                    KeyCode::Down => {
                        let i = state.selected().map(|i| i.saturating_add(1).min(item_strings.len() - 1));
                        state.select(i);
                    }
                    KeyCode::Up => {
                        let i = state.selected().map(|i| i.saturating_sub(1));
                        state.select(i);
                    }
                    KeyCode::Enter => {
                        selected = state.selected().map(|i| item_strings[i].clone());
                        break;
                    }
                    KeyCode::Esc | KeyCode::Char('q') => break,
                    _ => (),
                }
            }
        }
    }

    teardown_terminal(terminal)?;

    if let Some(sel) = selected {
        println!("Selected: {}", sel);
    }
    Ok(())
}

// Expanded chart with line chart added (using sparkline as approx), random data gen
fn render_chart(data: &str, chart_type: &str) -> Result<()> {
    let mut terminal = setup_terminal()?;

    let data_vec: Vec<u64> = if data.is_empty() {
        let mut rng = rand::thread_rng();
        (0..10).map(|_| rng.gen_range(0..100)).collect()
    } else {
        data.split(',').map(|d| d.trim().parse().unwrap_or(0)).collect()
    };

    terminal.draw(|f| {
        let block = Block::default().borders(Borders::ALL).title(format!("{} Chart", chart_type.to_uppercase()));
        match chart_type.to_lowercase().as_str() {
            "bar" => {
                let bars: Vec<(String, u64)> = data_vec.iter().enumerate().map(|(i, &v)| (i.to_string(), v)).collect();
                let bar_data: Vec<(&str, u64)> = bars.iter().map(|(s, v)| (s.as_str(), *v)).collect();
                let chart = BarChart::default()
                .block(block)
                .data(&bar_data)
                .bar_width(3)
                .bar_gap(1)
                .bar_style(Style::default().fg(Color::Cyan).bg(Color::Black))
                .value_style(Style::default().fg(Color::White).add_modifier(Modifier::BOLD));
                f.render_widget(chart, f.area());
            }
            "sparkline" | "line" => {
                let spark = Sparkline::default()
                .block(block)
                .data(&data_vec)
                .style(Style::default().fg(Color::Green).add_modifier(Modifier::DIM))
                .bar_set(ratatui::symbols::bar::NINE_LEVELS);
                f.render_widget(spark, f.area());
            }
            _ => {
                let p = Paragraph::new("Unsupported chart type");
                f.render_widget(p, f.area());
            }
        }
    })?;

    std::thread::sleep(Duration::from_secs(5));
    teardown_terminal(terminal)
}

// Expanded interactive app: more tabs, widgets, input form, live updates
#[derive(Clone, PartialEq)]
enum InputMode {
    Normal,
    Editing,
}

struct App {
    tab_index: usize,
    tabs: Vec<String>,
    input: String,
    input_mode: InputMode,
    scroll: u16,
    // More state: progress, list state, etc.
    progress: u16,
    list_state: ListState,
    items: Vec<String>,
}

impl App {
    fn new() -> App {
        let mut list_state = ListState::default();
        list_state.select(Some(0));
        App {
            tab_index: 0,
            tabs: vec!["Home".to_string(), "Input".to_string(), "List".to_string(), "Chart".to_string()],
            input: String::new(),
            input_mode: InputMode::Normal,
            scroll: 0,
            progress: 0,
            list_state,
            items: vec!["Item1".to_string(), "Item2".to_string(), "Item3".to_string(), "Item4".to_string()],
        }
    }

    fn update(&mut self, event: Event) -> bool {
        match event {
            Event::Key(key) if key.kind == KeyEventKind::Press => {
                match self.input_mode {
                    InputMode::Normal => match key.code {
                        KeyCode::Right | KeyCode::Tab => self.tab_index = (self.tab_index + 1) % self.tabs.len(),
                        KeyCode::Left => self.tab_index = self.tab_index.saturating_sub(1),
                        KeyCode::Char('e') if self.tab_index == 1 => self.input_mode = InputMode::Editing,
                        KeyCode::Down => self.scroll = self.scroll.saturating_add(1),
                        KeyCode::Up => self.scroll = self.scroll.saturating_sub(1),
                        KeyCode::Esc | KeyCode::Char('q') => return true, // Quit
                        _ => (),
                    },
                    InputMode::Editing => match key.code {
                        KeyCode::Char(c) => self.input.push(c),
                        KeyCode::Backspace => { self.input.pop(); }
                        KeyCode::Esc => self.input_mode = InputMode::Normal,
                        KeyCode::Enter => {
                            // Process input
                            self.input_mode = InputMode::Normal;
                            self.input.clear();
                        }
                        _ => (),
                    },
                }
            }
            _ => (),
        }
        false
    }

    fn tick(&mut self) {
        self.progress = (self.progress + 1) % 101;
    }

    fn view(&mut self, f: &mut Frame) {
        let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(3), Constraint::Min(1)])
        .split(f.area());

        let tabs = Tabs::new(self.tabs.iter().map(|t| Line::from(t.as_str())).collect::<Vec<Line>>())
        .block(Block::default().borders(Borders::ALL).title("Tabs"))
        .select(self.tab_index)
        .style(Style::default().fg(Color::White))
        .highlight_style(Style::default().add_modifier(Modifier::BOLD).fg(Color::Green));
        f.render_widget(tabs, vertical[0]);

        match self.tab_index {
            0 => { // Home: progress and welcome
                let chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
                .split(vertical[1]);

                let p = Paragraph::new("Welcome to Yuy TUI!\nPress q to quit.")
                .style(Style::default().fg(Color::Yellow))
                .block(Block::default().borders(Borders::ALL).title("Info"));
                f.render_widget(p, chunks[0]);

                let gauge = Gauge::default()
                .block(Block::default().borders(Borders::ALL).title("Live Progress"))
                .gauge_style(Style::default().fg(Color::Magenta))
                .percent(self.progress);
                f.render_widget(gauge, chunks[1]);
            }
            1 => { // Input form
                let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Length(3), Constraint::Min(0)])
                .split(vertical[1]);

                let input = Paragraph::new(self.input.as_str())
                .style(match self.input_mode {
                    InputMode::Normal => Style::default(),
                       InputMode::Editing => Style::default().fg(Color::Yellow),
                })
                .block(Block::default().borders(Borders::ALL).title("Input"));
                f.render_widget(input, chunks[0]);

                if self.input_mode == InputMode::Editing {
                    f.set_cursor_position((chunks[0].x + self.input.len() as u16 + 1, chunks[0].y + 1));
                }

                let instructions = Paragraph::new("Press 'e' to edit, Esc to stop.")
                .block(Block::default().borders(Borders::ALL).title("Instructions"));
                f.render_widget(instructions, chunks[1]);
            }
            2 => { // List
                let list_items: Vec<ListItem> = self.items.iter().enumerate().map(|(i, item)| {
                    ListItem::new(item.as_str()).style(if Some(i) == self.list_state.selected() { Style::default().fg(Color::Green) } else { Style::default() })
                }).collect();

                let list = List::new(list_items)
                .block(Block::default().borders(Borders::ALL).title("Selectable List"))
                .highlight_style(Style::default().add_modifier(Modifier::ITALIC).fg(Color::Blue))
                .highlight_symbol("-> ");
                f.render_stateful_widget(list, vertical[1], &mut self.list_state);
            }
            3 => { // Chart
                let data: Vec<u64> = (0..50).map(|i| i as u64 * self.progress as u64 / 100).collect();
                let spark = Sparkline::default()
                .block(Block::default().borders(Borders::ALL).title("Dynamic Sparkline"))
                .data(&data)
                .style(Style::default().fg(Color::Red));
                f.render_widget(spark, vertical[1]);
            }
            _ => (),
        }
    }
}

fn run_interactive_app(mode: &str) -> Result<()> {
    let mut terminal = setup_terminal()?;
    let mut app = App::new();
    let tick_rate = Duration::from_millis(100); // Faster for live updates
    let mut last_tick = Instant::now();

    // Mode-specific init
    if mode == "input" {
        app.tab_index = 1;
    } else if mode == "list" {
        app.tab_index = 2;
    }

    loop {
        terminal.draw(|f| app.view(f))?;

        let timeout = tick_rate.saturating_sub(last_tick.elapsed());
        if event::poll(timeout)? {
            let evt = event::read()?;
            if app.update(evt) {
                break;
            }
        }

        if last_tick.elapsed() >= tick_rate {
            app.tick();
            last_tick = Instant::now();
        }
    }

    teardown_terminal(terminal)
}

// Exported functions for C FFI

#[no_mangle]
pub extern "C" fn yuy_render_text(content: *const c_char, color: *const c_char, style: *const c_char) -> c_int {
    let content_str = unsafe { CStr::from_ptr(content).to_str().unwrap_or("") };
    let color_str = unsafe { CStr::from_ptr(color).to_str().unwrap_or("white") };
    let style_str = unsafe { CStr::from_ptr(style).to_str().unwrap_or("none") };
    if render_text(content_str, color_str, style_str).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_render_table(headers: *const c_char, rows: *const c_char) -> c_int {
    let headers_str = unsafe { CStr::from_ptr(headers).to_str().unwrap_or("") };
    let rows_str = unsafe { CStr::from_ptr(rows).to_str().unwrap_or("") };
    if render_table(headers_str, rows_str).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_render_progress(percent: u16, label: *const c_char) -> c_int {
    let label_str = unsafe { CStr::from_ptr(label).to_str().unwrap_or("Progress") };
    if render_progress(percent, label_str).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_render_spinner(duration: u64, message: *const c_char) -> c_int {
    let message_str = unsafe { CStr::from_ptr(message).to_str().unwrap_or("Loading...") };
    if render_spinner(duration, message_str).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_render_syntax(code: *const c_char, lang: *const c_char) -> c_int {
    let code_str = unsafe { CStr::from_ptr(code).to_str().unwrap_or("") };
    let lang_str = unsafe { CStr::from_ptr(lang).to_str().unwrap_or("rust") };
    if render_syntax(code_str, lang_str).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_render_markdown(content: *const c_char) -> c_int {
    let content_str = unsafe { CStr::from_ptr(content).to_str().unwrap_or("") };
    if render_markdown(content_str).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_render_list(items: *const c_char, selectable: bool) -> c_int {
    let items_str = unsafe { CStr::from_ptr(items).to_str().unwrap_or("") };
    if render_list(items_str, selectable).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_render_chart(data: *const c_char, chart_type: *const c_char) -> c_int {
    let data_str = unsafe { CStr::from_ptr(data).to_str().unwrap_or("") };
    let chart_type_str = unsafe { CStr::from_ptr(chart_type).to_str().unwrap_or("bar") };
    if render_chart(data_str, chart_type_str).is_ok() {
        0
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn yuy_run_interactive_app(mode: *const c_char) -> c_int {
    let mode_str = unsafe { CStr::from_ptr(mode).to_str().unwrap_or("demo") };
    if run_interactive_app(mode_str).is_ok() {
        0
    } else {
        -1
    }
}
