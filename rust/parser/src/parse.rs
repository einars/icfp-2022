use crate::*;

use nom::{
    branch::alt,
    bytes::complete::{tag, is_not},
    character::complete::{char, digit1, newline, one_of},
    combinator::recognize,
    sequence::delimited,
    multi::{many0, many1, separated_list0, separated_list1},
    IResult,
};

fn match_decimal(s: &str) -> IResult<&str, u32> {
    recognize(many1(digit1))(s)
        .map(|(rest, s)| (rest, s.parse::<u32>().unwrap()))
}

fn match_point(s: &str) -> IResult<&str, (u32, u32)> {
    let rest = s;
    let (rest, _) = char('[')(rest)?;
    let (rest, x) = match_decimal(rest)?;
    let (rest, _) = char(',')(rest)?;
    let (rest, y) = match_decimal(rest)?;
    let (rest, _) = char(']')(rest)?;
    Ok((rest, (x, y)))
}
fn match_cutdirection(s: &str) -> IResult<&str, CutDirection> {

    let (rest, o) = delimited(char('['), one_of("xyXY"), char(']'))(s)?;

    match o {
        'X' | 'x' => Ok((rest, CutDirection::X)),
        'Y' | 'y' => Ok((rest, CutDirection::Y)),
        _ => panic!("should not happen")
    }
}

fn match_block(s: &str) -> IResult<&str, BlockId> {
    delimited(
        char('['), 
        separated_list1(tag("."), match_decimal),
        char(']')
    )(s)
        .map(|(rest, vd)| (rest, BlockId(vd)))
}

fn match_color_inside(s: &str) -> IResult<&str, Color> {
    let rest = s;
    let (rest, r) = match_decimal(rest)?;
    let (rest, _) = tag(",")(rest)?;
    let (rest, g) = match_decimal(rest)?;
    let (rest, _) = tag(",")(rest)?;
    let (rest, b) = match_decimal(rest)?;
    let (rest, _) = tag(",")(rest)?;
    let (rest, a) = match_decimal(rest)?;

    Ok((rest, Color([
        r.try_into().unwrap(),
        g.try_into().unwrap(),
        b.try_into().unwrap(),
        a.try_into().unwrap(),
    ])))
}

fn match_color(s: &str) -> IResult<&str, Color> {
    delimited(tag("["), match_color_inside, tag("]"))(s)
}


fn cmd_comment(s: &str) -> IResult<&str, ProgCmd> {
    let rest = s;
    let (rest, _) = char('#')(rest)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, s) = is_not("\n")(rest)?;
    Ok((rest, ProgCmd::Comment(s.to_string())))
}


fn cmd_pointcut(s: &str) -> IResult<&str, ProgCmd> {
    let (rest, _) = tag("cut")(s)?;

    let (rest, block) = delimited(
        many0(char(' ')),
        match_block,
        many0(char(' '))
    )(rest)?;

    let (rest, point) = match_point(rest)?;
    Ok((rest, ProgCmd::PointCut(block, point)))
}

fn cmd_linecut(s: &str) -> IResult<&str, ProgCmd> {
    let (rest, _) = tag("cut")(s)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, block) = match_block(rest)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, o) = match_cutdirection(rest)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, decimal) = delimited(
        char('['),
        match_decimal,
        char(']')
    )(rest)?;

    Ok((rest, ProgCmd::LineCut(block, o, decimal)))
}


fn cmd_swap(s: &str) -> IResult<&str, ProgCmd> {
    let (rest, _) = tag("swap")(s)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, b1) = match_block(rest)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, b2) = match_block(rest)?;
    Ok((rest, ProgCmd::Swap(b1, b2)))
}

fn cmd_merge(s: &str) -> IResult<&str, ProgCmd> {
    let (rest, _) = tag("merge")(s)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, b1) = match_block(rest)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, b2) = match_block(rest)?;
    Ok((rest, ProgCmd::Merge(b1, b2)))
}

fn cmd_color(s: &str) -> IResult<&str, ProgCmd> {
    let (rest, _) = tag("color")(s)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, b) = match_block(rest)?;
    let (rest, _) = many0(char(' '))(rest)?;
    let (rest, color) = match_color(rest)?;
    Ok((rest, ProgCmd::Color(b, color)))
}



pub fn parse(s: &str) -> Result<Vec<ProgCmd>, String> {

    let sl = separated_list0(newline, alt((
        cmd_comment,
        cmd_pointcut,
        cmd_linecut,
        cmd_color,
        cmd_swap,
        cmd_merge,
    )))(s);


    match sl {
        Ok((rest, prg)) => {
            if rest.len() == 0 {
                Ok(prg)
            } else {
                let e = format!("Cannot parse: {}", rest);
                Err(e)
            }
        },
        Err(e) => {
            let e = format!("Parse error: {}", e);
            Err(e)
        }

    }
}

