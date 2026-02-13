use anyhow::Result;
use clap::{Parser, Subcommand};
use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use markdown::to_mdast;
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{
        BarChart, Block, Borders, Cell, Gauge, List, ListItem, ListState, Paragraph, Row, Sparkline, Table,
        Tabs,
    },
    Frame, Terminal,
};
use std::io::{self, stdout, Write};
use std::time::{Duration, Instant};
use syntect::{
    easy::HighlightLines,
    parsing::SyntaxSet,
    highlighting::{Theme, ThemeSet},
};

#[derive(Parser)]
#[command(name = "yuy-lib", about = "Vibrant UI library for Hacker Lang (inspired by Bubble Tea, Rich, Ratatui)")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Render colored/styled text (Rich-inspired)
    Text {
        /// Text to render
        #[arg(long)]
        content: String,
        /// Color (e.g., red, green, #FF0000)
        #[arg(long, default_value = "white")]
        color: String,
        /// Style (bold, italic, underline)
        #[arg(long, default_value = "none")]
        style: String,
    },
    /// Render a table (Rich/Ratatui-inspired)
    Table {
        /// Headers (comma-separated)
        #[arg(long)]
        headers: String,
        /// Rows (pipe-separated rows, comma-separated cells, e.g., "row1col1,row1col2|row2col1,row2col2")
        #[arg(long)]
        rows: String,
    },
    /// Render a progress bar/gauge (Rich/Ratatui-inspired)
    Progress {
        /// Progress percentage (0-100)
        #[arg(long)]
        percent: u16,
        /// Label
        #[arg(long, default_value = "Progress")]
        label: String,
    },
    /// Render a spinner (Bubble Tea-inspired)
    Spinner {
        /// Duration in seconds
        #[arg(long, default_value = "5")]
        duration: u64,
        /// Message
        #[arg(long, default_value = "Loading...")]
        message: String,
    },
    /// Syntax highlighting for code (Rich-inspired)
    Syntax {
        /// Code snippet
        #[arg(long)]
        code: String,
        /// Language (e.g., rust, python)
        #[arg(long, default_value = "rust")]
        lang: String,
    },
    /// Render markdown (Rich-inspired)
    Markdown {
        /// Markdown content
        #[arg(long)]
        content: String,
    },
    /// Render a list (Bubble Tea/Ratatui-inspired)
    List {
        /// Items (comma-separated)
        #[arg(long)]
        items: String,
        /// Selectable (true/false)
        #[arg(long, default_value = "false")]
        selectable: bool,
    },
    /// Render a chart (Ratatui-inspired)
    Chart {
        /// Data (comma-separated numbers)
        #[arg(long)]
        data: String,
        /// Type (bar, line, sparkline)
        #[arg(long, default_value = "bar")]
        chart_type: String,
    },
    /// Full interactive TUI app (Bubble Tea/Elm-inspired)
    Interactive {
        /// App mode (demo, input, etc.)
        #[arg(long, default_value = "demo")]
        mode: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Text { content, color, style } => render_text(&content, &color, &style),
        Commands::Table { headers, rows } => render_table(&headers, &rows),
        Commands::Progress { percent, label } => render_progress(percent, &label),
        Commands::Spinner { duration, message } => render_spinner(duration, &message),
        Commands::Syntax { code, lang } => render_syntax(&code, &lang),
        Commands::Markdown { content } => render_markdown(&content),
        Commands::List { items, selectable } => render_list(&items, selectable),
        Commands::Chart { data, chart_type } => render_chart(&data, &chart_type),
        Commands::Interactive { mode } => run_interactive_app(&mode),
    }
}

fn parse_color(color: &str) -> Color {
    match color.to_lowercase().as_str() {
        "red" => Color::Red,
        "green" => Color::Green,
        "blue" => Color::Blue,
        // Add more or parse hex
        _ => Color::White,
    }
}

fn parse_style(style: &str) -> Modifier {
    match style.to_lowercase().as_str() {
        "bold" => Modifier::BOLD,
        "italic" => Modifier::ITALIC,
        "underline" => Modifier::UNDERLINED,
        _ => Modifier::empty(),
    }
}

fn render_text(content: &str, color: &str, style_str: &str) -> Result<()> {
    let style = Style::default().fg(parse_color(color)).add_modifier(parse_style(style_str));
    println!("{}", Span::styled(content, style));
    Ok(())
}

fn render_table(headers: &str, rows: &str) -> Result<()> {
    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.draw(|f| {
        let header_cells = headers.split(',').map(|h| Cell::from(h.trim()));
        let header = Row::new(header_cells).style(Style::default().add_modifier(Modifier::BOLD));
        let rows_vec: Vec<Row> = rows.split('|').map(|r| {
            let cells = r.split(',').map(|c| Cell::from(c.trim()));
            Row::new(cells)
        }).collect();
        let widths = vec![Constraint::Percentage(50), Constraint::Percentage(50)]; // Adjust as needed
        let table = Table::new(rows_vec, widths)
            .header(header)
            .block(Block::default().borders(Borders::ALL).title("Table"));
        f.render_widget(table, f.area());
    })?;
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    Ok(())
}

fn render_progress(percent: u16, label: &str) -> Result<()> {
    // Similar to table, use Ratatui for gauge
    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.draw(|f| {
        let gauge = Gauge::default()
            .block(Block::default().borders(Borders::ALL).title(label))
            .gauge_style(Style::default().fg(Color::Green))
            .percent(percent.clamp(0, 100));
        f.render_widget(gauge, f.area());
    })?;
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    Ok(())
}

fn render_spinner(duration: u64, message: &str) -> Result<()> {
    let start = Instant::now();
    let frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
    while start.elapsed() < Duration::from_secs(duration) {
        for frame in &frames {
            print!("\r{} {}", frame, message);
            io::stdout().flush()?;
            std::thread::sleep(Duration::from_millis(80));
        }
    }
    println!("\rDone! ");
    Ok(())
}

fn render_syntax(code: &str, lang: &str) -> Result<()> {
    let ss = SyntaxSet::load_defaults_newlines();
    let syntax = ss.find_syntax_by_extension(lang).unwrap_or_else(|| ss.find_syntax_plain_text());
    let ts = ThemeSet::load_defaults();
    let theme: &Theme = &ts.themes["base16-ocean.dark"];
    let mut h = HighlightLines::new(syntax, theme);
    for line in code.lines() {
        let ranges = h.highlight_line(line, &ss)?;
        for (style, text) in ranges {
            let fg = style.foreground;
            print!("\x1b[38;2;{};{};{}m{}\x1b[0m", fg.r, fg.g, fg.b, text);
        }
        println!();
    }
    Ok(())
}

fn render_markdown(content: &str) -> Result<()> {
    let md = to_mdast(content, &markdown::ParseOptions::default()).map_err(|e| anyhow::anyhow!("Failed to parse markdown: {:?}", e))?;
    // Simple rendering - expand for full Rich-like
    println!("{:#?}", md); // Placeholder: convert to styled text
    Ok(())
}

fn render_list(items: &str, selectable: bool) -> Result<()> {
    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let item_strings: Vec<String> = items.split(',').map(|i| i.trim().to_string()).collect();
    let list_items: Vec<ListItem> = item_strings.iter().map(|i| ListItem::new(i.as_str())).collect();
    let mut state = ListState::default();
    terminal.draw(|f| {
        let list = List::new(list_items.clone())
            .block(Block::default().borders(Borders::ALL).title("List"))
            .highlight_style(Style::default().add_modifier(Modifier::BOLD).fg(Color::Green));
        f.render_stateful_widget(list, f.area(), &mut state);
    })?;
    if selectable {
        // Wait for selection (simple key handler)
        if let Event::Key(key) = event::read()? {
            if key.kind == KeyEventKind::Press {
                match key.code {
                    KeyCode::Down => state.select(Some(state.selected().unwrap_or(0) + 1)),
                    KeyCode::Up => state.select(Some(state.selected().unwrap_or(0).saturating_sub(1))),
                    KeyCode::Enter => println!("Selected: {}", item_strings[state.selected().unwrap_or(0)]),
                    KeyCode::Esc => (),
                    _ => (),
                }
            }
        }
    }
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    Ok(())
}

fn render_chart(data: &str, chart_type: &str) -> Result<()> {
    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let data_vec: Vec<u64> = data.split(',').map(|d| d.trim().parse().unwrap_or(0)).collect();
    terminal.draw(|f| {
        let block = Block::default().borders(Borders::ALL).title("Chart");
        match chart_type {
            "bar" => {
                let bars: Vec<(String, u64)> = data_vec.iter().enumerate().map(|(i, &v)| (i.to_string(), v)).collect();
                let bar_data: Vec<(&str, u64)> = bars.iter().map(|(s, v)| (s.as_str(), *v)).collect();
                let chart = BarChart::default()
                    .block(block)
                    .data(&bar_data)
                    .bar_width(3)
                    .bar_style(Style::default().fg(Color::Cyan));
                f.render_widget(chart, f.area());
            }
            "sparkline" => {
                let spark = Sparkline::default()
                    .block(block)
                    .data(&data_vec)
                    .style(Style::default().fg(Color::Green));
                f.render_widget(spark, f.area());
            }
            _ => (), // Add line chart if needed
        }
    })?;
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    Ok(())
}

// Bubble Tea-inspired interactive app with model-update-view
struct App {
    tab_index: usize,
    tabs: Vec<String>,
    // Add more state for Elm-like model
}

impl App {
    fn new() -> App {
        App {
            tab_index: 0,
            tabs: vec!["Tab1".to_string(), "Tab2".to_string(), "Tab3".to_string()],
        }
    }

    fn update(&mut self, key: KeyCode) {
        match key {
            KeyCode::Right => self.tab_index = (self.tab_index + 1) % self.tabs.len(),
            KeyCode::Left => self.tab_index = self.tab_index.saturating_sub(1),
            _ => (),
        }
    }

    fn view(&self, f: &mut Frame) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(3), Constraint::Min(0)])
            .split(f.area());
        let tabs = Tabs::new(self.tabs.iter().cloned().map(Line::from).collect::<Vec<_>>())
            .block(Block::default().borders(Borders::ALL).title("Tabs"))
            .select(self.tab_index)
            .highlight_style(Style::default().add_modifier(Modifier::BOLD).fg(Color::Green));
        f.render_widget(tabs, chunks[0]);
        // Content per tab (demo)
        let content = Paragraph::new(format!("Content for {}", self.tabs[self.tab_index]))
            .block(Block::default().borders(Borders::ALL));
        f.render_widget(content, chunks[1]);
    }
}

fn run_interactive_app(_mode: &str) -> Result<()> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let mut app = App::new();
    let tick_rate = Duration::from_millis(250);
    let mut last_tick = Instant::now();
    loop {
        terminal.draw(|f| app.view(f))?;
        let timeout = tick_rate.saturating_sub(last_tick.elapsed());
        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    match key.code {
                        KeyCode::Esc | KeyCode::Char('q') => break,
                        _ => app.update(key.code),
                    }
                }
            }
        }
        if last_tick.elapsed() >= tick_rate {
            // Update live if needed
            last_tick = Instant::now();
        }
    }
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    Ok(())
}
