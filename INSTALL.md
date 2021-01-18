#  WebCaml install instructions for Windows.

1. Download and unzip file
2. Install and configure required dependencies XMing and PuTTy
3. Inside the WSL, run commands `sudo apt install pkg-config` and `sudo apt install libcurl4-gnutls-dev` to install required external dependencies
4. With opam install ANSITerminal, Graphics, and OCurl
5. Run "export DISPLAY=:0" to enable display
6. Open GUI with 'make gui'


# WebCaml install instructions for Mac
1. Download and unzip file
2. Download packages ANSITerminal, Graphics, and OCurl
(Graphics requires either XQuartz (Mac) or XMing (Windows))
3. Open GUI by typing 'make gui' in the command line
