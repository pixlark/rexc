fn main() {
    let project_dir = directories::ProjectDirs::from("", "", "rexc");
    match project_dir {
        // TODO(Brooke): Technically this doesn't handle UTF8-invalid paths.
        //               Do we care about that in the slightest?
        Some(dir) => println!("{}", dir.data_dir().display()),
        None => std::process::exit(1),
    }
}
