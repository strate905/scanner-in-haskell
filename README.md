# Educational Network Scanner (Haskell)

This project implements a small, readable network scanner intended for teaching. It follows the requirements in `SPECIFICATIONS.md` and sticks to user‑space techniques so it can run without special privileges.

**Educational Focus:** The source code is extensively commented to help students learn Haskell and network programming concepts. Key topics covered include:
- Concurrent programming with async and semaphores
- TCP/UDP socket programming and network protocols
- Functional error handling (Maybe, Either, IO)
- Resource management with bracket
- Rate limiting and retry strategies
- Bit manipulation for network address formatting

See `AGENTS.md` for contributor guidelines and workflow expectations.

## Features

- TCP connect() scanning of a default “top 25” port set with optional custom lists.
- UDP probing with the “silence ⇒ open|filtered” convention and ICMP 3/3 detection.
- Concurrency control, per-attempt timeouts, retry budget, and optional global rate limiting.
- DNS resolution with `--no-dns` escape hatch for numeric targets.
- Markdown table output (default) or structured JSON via `--json`.

## Building

The project uses Cabal. From the repository root:

```bash
cabal build
```

Run the executable with:

```bash
cabal run scanner -- --help
```

To execute an actual scan (defaults to top TCP/UDP ports), supply a target:

```bash
cabal run scanner -- 127.0.0.1
```

After a successful build you can also run the compiled binary directly (located in `dist-newstyle`), for example:

```bash
./dist-newstyle/build/*/ghc-*/scanner-0.1.0.0/x/scanner/build/scanner/scanner localhost
```

If Cabal or required libraries are not installed on your system, install `cabal-install` and ensure `ghc` can find the dependencies listed in `scanner.cabal`. When working completely offline you must pre-populate your Cabal store with those packages, otherwise the build will fail when Cabal tries to download them.

> **Tip for sandboxed environments:** if you do not have write access to the default Cabal config/cache locations, point them inside the project, e.g.
> `CABAL_DIR=$PWD/cabal-home XDG_CACHE_HOME=$PWD/cabal-home/cache cabal --config-file=cabal-home/config build`.

## Usage

```
scanner [--tcp PORTS] [--udp PORTS] [--timeout SECONDS] \
        [--retries N] [--concurrency N] [--rate PPS] \
        [--no-dns] [--json] <target>
```

- `PORTS` accepts comma/ range syntax (e.g. `80,443,1000-1005`). Use `none` to disable a protocol entirely.
- `<target>` defaults to `127.0.0.1` when omitted.
- Run `cabal run scanner -- --help` to see every flag along with the educational notes.

Example Markdown output:

```markdown
Target: example.com (93.184.216.34)
| Port | Proto | State        | Service    | Reason                     |
|-----:|:-----:|:-------------|:----------|:---------------------------|
|   22 |  TCP  | open         | ssh        | connect() succeeded        |
|   53 |  UDP  | open|filtered| dns        | no reply (no ICMP)         |
```

Example JSON:

```bash
scanner --json example.com | jq
```

## Testing

- `cabal build` verifies the project compiles.
- `cabal run scanner -- --help` prints all supported flags.
- `cabal run scanner -- localhost` runs the default scan against your own host—safe for experimentation.
- `scripts/smoke-test.sh` executes the steps above automatically (Markdown + JSON runs).

All of these commands require the dependencies (`async`, `network`, `aeson`, `optparse-applicative`, etc.) to be available in your Cabal store. If you are offline, ensure the packages are already cached locally.

## Learning from the Code

For students and learners:

1. **Start with `app/Main.hs`**: The source code includes detailed educational comments explaining each concept.
2. **Look for "Educational note:" markers**: These highlight important patterns and explain tricky sections.
3. **Key sections to study**:
   - Rate limiting implementation (lines ~113-149): Demonstrates MVar-based coordination
   - TCP vs UDP scanning (lines ~419-499): Shows the fundamental differences between protocols
   - Error classification (lines ~513-552): Explains how to interpret network errors
   - IPv6 address formatting (lines ~619-644): Demonstrates bit manipulation
   - Retry logic (lines ~386-417): Shows recursive patterns with state

## Notes

- UDP scanning is notoriously ambiguous. The tool explains this in both the CLI help and table footer to reinforce the lesson.
- Service guesses use a tiny IANA-derived lookup table; they are hints, not guarantees.
- The code prioritizes clarity over performance—it's designed to be read and understood.

Happy scanning and, more importantly, happy learning!

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
