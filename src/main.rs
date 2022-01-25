//! Default Compute@Edge template program.

use fastly::http::{header, Method, StatusCode};
use fastly::{mime, Error, Request, Response};
use serde_json::{json, from_str, Value};

// test json parser

#[fastly::main]
fn main(req: Request) -> Result<Response, Error> {

    // Pattern match on the path...
    match req.get_path() {

        "/json" => {

            match req.get_method() {
                // Allow POST requests.
                &Method::POST | &Method::PUT  => (),

                // Deny anything else.
                _ => {
                    return Ok(Response::from_status(StatusCode::METHOD_NOT_ALLOWED)
                              .with_header(header::ALLOW, "POST, PUT")
                              .with_body_text_plain("This method is not allowed\n"))
                }
            };

            let body = req.into_body_str();

            println!("body of request {} ", body);

            // Parse the string of data into serde_json::Value.
            let v: Value = from_str(&body)?;

            println!("translated to json {} ", v);

            Ok(Response::new()
                .with_body_json(&v)?)
        }

        // Catch all other requests and return a 404.
        _ => Ok(Response::from_status(StatusCode::NOT_FOUND)
            .with_body_text_plain("The page you requested could not be found\n")),
    }
}
