extern crate getopts;
use adap_haff::*;
use getopts::Options;
use std::fmt::{self, Display};
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::str::FromStr;
use std::{thread, time};

fn vec_fill(v: &mut Vec<u8>) {
    while v.len() < 8 {
        v.push(0);
    }
}

fn char2bit(c: char) -> [u8; 8] {
    let mut res = [0u8; 8];
    let mut mask = 0b10000000u8;
    for i in 0..8 {
        res[i] = if c as u8 & mask > 0 { 1 } else { 0 };
        mask >>= 1;
    }
    res
}

fn bit_writer(
    b: u8,
    writer: &mut BufWriter<File>,
    block: &mut Vec<u8>,
) -> Result<(), Box<dyn std::error::Error>> {
    block.push(b);

    if block.len() == 8 {
        let mut bit = 0b10000000u8;
        let mut res = 0;
        for i in 0..8 {
            // print!("{}", block[i]);
            if block[i] == 1 {
                res += bit;
            }
            bit >>= 1;
        }
        writer.write_all(&[res])?;
        *block = Vec::new();
    }

    Ok(())
}

fn encode(
    filename: &str,
    animation: usize,
    mut verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if animation > 0 {
        verbose = true;
    }

    let mut reader = BufReader::new(File::open(filename)?);
    let mut output = BufWriter::new(File::create(format!("{}.hufb", filename).as_str())?);
    let mut buf = [0; 1];

    let mut encode_tree = AdapHuffTree::new(HuffTreeType::Encoder, '#');
    let mut block = Vec::new();

    let mut ani_str = String::new();
    let mut up_num = 0;
    if animation > 0 {
        print!("\x1b[2J\x1b[100A");
        std::io::stdout().flush().unwrap();
    }

    loop {
        let mut tmp_str = String::new();
        if animation > 0 {
            thread::sleep(time::Duration::from_millis(animation as u64));
            print!("\x1b[{}A\x1b[0J", up_num + 1);
            std::io::stdout().flush().unwrap();
            up_num = 0;
        }

        let mut _c_k = '_';
        let c_w = match reader.read(&mut buf)? {
            0 => None,
            _ => {
                _c_k = buf[0] as char;
                Some(&_c_k)
            }
        };
        let break_flag = c_w.is_none();
        if verbose {
            let tree = encode_tree.get_tree_figure();
            println!("{}", tree);
            up_num += tree
                .chars()
                .filter(|&c| c == '\n')
                .collect::<Vec<_>>()
                .len();
        }
        let (v, exist) = encode_tree.encode(c_w);
        v.into_iter()
            .map(|b| {
                if verbose {
                    let bs = b.to_string();
                    tmp_str.push_str(&bs);
                    ani_str.push_str(&bs);
                }
                bit_writer(b, &mut output, &mut block)
            })
            .collect::<Result<_, _>>()?;
        if !exist {
            let c = *c_w.unwrap();
            let arr = char2bit(c);
            arr.iter()
                .map(|b| {
                    if verbose {
                        let bs = b.to_string();
                        tmp_str.push_str(&bs);
                        ani_str.push_str(&bs);
                    }
                    bit_writer(*b, &mut output, &mut block)
                })
                .collect::<Result<_, _>>()?;
        }
        if verbose {
            println!(
                "{}: {}\n{}",
                if !break_flag {
                    format!("{:?}", buf[0] as char)
                } else {
                    "EOF".to_string()
                },
                tmp_str,
                ani_str
            );
            up_num += 2;
        }

        if break_flag {
            break;
        }
    }

    if block.len() > 0 {
        vec_fill(&mut block);
        let mut bit = 0b10000000u8;
        let mut res = 0;
        for i in 0..8 {
            // print!("{}", block[i]);
            if block[i] == 1 {
                res += bit;
            }
            bit >>= 1;
        }
        output.write_all(&[res])?;
    }

    // println!("\n{}", encode_tree.get_tree_figure());

    // output.write_all(&write_buf)?;
    output.flush()?;

    if verbose {
        /*
        let tree = encode_tree.get_tree_figure();
        println!("{}", tree);
        */
        println!(
            "\nentropy:\n{}\naverage length:\n{}",
            encode_tree.get_entropy(),
            encode_tree.get_avg_len()
        );
    }

    Ok(())
}

fn u82vec(block: u8) -> Vec<u8> {
    let mut mask = 0b10000000u8;

    let mut res = Vec::new();
    for _ in 0..8 {
        let b = if block & mask > 0 { 1 } else { 0 };
        res.push(b);
        mask >>= 1;
    }

    res
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct CharFromBits(char);

impl Display for CharFromBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for CharFromBits {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 8 {
            return Err(());
        }

        let mut bit = 0b10000000u8;
        let mut res = 0u8;
        for b in s.chars().into_iter() {
            if b == '1' {
                res += bit;
            }
            bit >>= 1;
        }

        Ok(Self(res as char))
    }
}

fn decode(
    filename: &str,
    animation: usize,
    mut verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if animation > 0 {
        verbose = true;
    }

    let mut reader = BufReader::new(File::open(filename)?);
    let mut output = BufWriter::new(File::create(format!("{}.dec", filename).as_str())?);
    let mut buf = [0; 1];

    let mut decode_tree = AdapHuffTree::new(HuffTreeType::Decoder, CharFromBits('#'));
    decode_tree.set_word_validation(Box::new(|v: &Vec<char>| v.len() == 8));

    let mut ani_str = String::new();
    let mut up_num = 0;
    if animation > 0 {
        print!("\x1b[2J\x1b[100A");
        std::io::stdout().flush().unwrap();
    }

    loop {
        match reader.read(&mut buf)? {
            0 => break,
            _ => {
                let bit_v = u82vec(buf[0]);
                for b in bit_v.into_iter() {
                    let c = char::from_str(format!("{}", b).as_str()).unwrap();
                    // print!("{}", c);
                    let chara_w = decode_tree.decode(c);
                    if let Some(chara) = chara_w {
                        write!(output, "{}", chara.0)?;

                        if animation > 0 {
                            thread::sleep(time::Duration::from_millis(animation as u64));
                            print!("\x1b[{}A\x1b[0J", up_num + 1);
                            std::io::stdout().flush().unwrap();
                            up_num = 0;
                        }

                        if verbose {
                            let tree = decode_tree.get_tree_figure();
                            println!("{}", tree);
                            up_num += tree
                                .chars()
                                .filter(|&c| c == '\n')
                                .collect::<Vec<_>>()
                                .len();
                            let c = chara.0.to_string();
                            ani_str.push_str(&c);
                            println!("{:?}\n{}", c, ani_str);
                            up_num += 2;
                        }
                    }
                }
            }
        }
    }

    // println!("");

    output.flush()?;

    if verbose {
        /*
        let tree = decode_tree.get_tree_figure();
        println!("{}", tree);
         */
        println!(
            "\nentropy:\n{}\naverage length:\n{}",
            decode_tree.get_entropy(),
            decode_tree.get_avg_len()
        );
    }

    Ok(())
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = std::env::args().collect::<Vec<_>>();
    let program = args[0].clone();
    let mut opts = Options::new();
    opts.optflag("d", "decode", "decode mode.");
    opts.optflag("v", "verbose", "verbose mode.");
    let ani_speed_def = 500;
    opts.optflagopt(
        "a",
        "animation",
        "animation mode.",
        format!("speed(ms, d={})", ani_speed_def).as_str(),
    );
    opts.optflag("h", "help", "print this help menu.");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };
    if matches.opt_present("h") || matches.free.is_empty() {
        print_usage(&program, opts);
        return Err(Box::new(std::io::Error::new(std::io::ErrorKind::Other, "")));
    }
    let filename = matches.free[0].as_str();
    let ani = matches.opt_str("a").map_or(
        if matches.opt_present("a") {
            ani_speed_def
        } else {
            0
        },
        |s| s.parse::<usize>().unwrap_or(ani_speed_def),
    );
    let ver = matches.opt_present("v");
    if !matches.opt_present("d") {
        encode(filename, ani, ver)?;
    } else {
        decode(filename, ani, ver)?;
    }

    Ok(())
}
