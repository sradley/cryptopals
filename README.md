# Cryptopals
This repository holds all my solutions to the cryptopals cryptography
challenges. 

## Usage
```
$ cabal run -v0 cryptopals
Usage: cabal run cryptopals <number>
```
```
$ cabal run -v0 cryptopals 1
"SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
```

## Challenges
    
### Set 01: Basics
 - [X] Set 01, Challenge 01: Convert Hex to Base64
 - [X] Set 01, Challenge 02: Implement Fixed XOR
 - [X] Set 01, Challenge 03: Single-Byte XOR Cipher
 - [X] Set 01, Challenge 04: Detect Single-Byte XOR
 - [X] Set 01, Challenge 05: Implement Repeating-Key XOR
 - [X] Set 01, Challenge 06: Break Repeating-Key XOR
 - [X] Set 01, Challenge 07: AES in ECB Mode
 - [X] Set 01, Challenge 08: Detect AES in ECB Mode
    
### Set 02: Block Crypto
 - [X] Set 02, Challenge 09: Implement PKCS#7 Padding
 - [X] Set 02, Challenge 10: Implement AES in CBC Mode
 - [X] Set 02, Challenge 11: An ECB/CBC Detection Oracle
 - [ ] Set 02, Challenge 12: Byte-at-a-time ECB Decryption (Simple)
 - [ ] Set 02, Challenge 13: Cut-and-paste ECB Decryption
 - [ ] Set 02, Challenge 14: Byte-at-a-time ECB Decryption (Harder)
 - [ ] Set 02, Challenge 15: PKCS#7 Padding Validation
 - [ ] Set 02, Challenge 16: CBC Bitflipping Attacks

### Set 03: Block & Stream Crypto
 - [ ] Set 03, Challenge 17: A CBC Padding oracle
 - [ ] Set 03, Challenge 18: Implement AES in CTR Mode
 - [ ] Set 03, Challenge 19: Break Fixed-Nonce CTR with Substitutions
 - [ ] Set 03, Challenge 20: Break Fixed-Nonce CTR with Statistics
 - [ ] Set 03, Challenge 21: Implement the MT19937 Mersenne Twister RNG
 - [ ] Set 03, Challenge 22: Crack an MT19937 Seed
 - [ ] Set 03, Challenge 23: Clone an MT19937 RNG from its Output
 - [ ] Set 03, Challenge 24: Create the MT19937 Stream Cipher & Break it
            
### Set 04: Stream Crypto & Randomness
 - [ ] Set 04, Challenge 25: Break "Random Access Read/Write" AES in CTR
 - [ ] Set 04, Challenge 26: CTR Bitflipping Attacks
 - [ ] Set 04, Challenge 27: Recover the Key from CBC with IV=Key
 - [ ] Set 04, Challenge 28: Implement SHA-1 Keyed MAC
 - [ ] Set 04, Challenge 29: Break SHA-1 Keyed Mac with Length Extension
 - [ ] Set 04, Challenge 30: Break MD4 Keyed MAC with Length Extension
 - [ ] Set 04, Challenge 31: Implement and Break HMAC-SHA1 with an
                             Artificial Timing Leak
 - [ ] Set 04, Challenge 32: Break HMAC-SHA1 with a slightly less
                             Artificial Timing Leak
    
### Set 05: Diffie-Hellman & Friends
 - [ ] Set 05, Challenge 33: Implement Diffie-Hellman
 - [ ] Set 05, Challenge 34: Implement a MITM Key-Fixing Attack on
                             Diffie-Hellman with Parameter Injection
 - [ ] Set 05, Challenge 35: Implement DH with Negotiated Groups, and
                             Break with Malicious "g" Parameters
 - [ ] Set 05, Challenge 36: Implement Secure Remote Password (SRP)
 - [ ] Set 05, Challenge 37: Break SRP with Zero Key
 - [ ] Set 05, Challenge 38: Offline Dictionary Attack on Simplified SRP
 - [ ] Set 05, Challenge 39: Implement RSA
 - [ ] Set 05, Challenge 40: Implement an E=3 RSA Broadcast Attack
    
### Set 06: RSA & DSA
 - [ ] Set 06, Challenge 41: Implement an Unpadded Message Recovery Oracle
 - [ ] Set 06, Challenge 42: Bleichenbacher's E=3 RSA Attack
 - [ ] Set 06, Challenge 43: DSA Key Recovery from Nonce
 - [ ] Set 06, Challenge 44: DSA Nonce Recovery from Repeated Nonce
 - [ ] Set 06, Challenge 45: DSA Parameter Tampering
 - [ ] Set 06, Challenge 46: An RSA Parity Oracle
 - [ ] Set 06, Challenge 47: Bleichenbacher's PKCS#1.5 Padding Oracle
                             (Simple Case)
 - [ ] Set 06, Challenge 48: Bleichenbacher's PKCS#1.5 Padding Oracle
                             (Complete Case)
    
### Set 07: Hashes  
 - [ ] Set 07, Challenge 49: CBC-MAC Message Forgery
 - [ ] Set 07, Challenge 50: Hashing with CBC-MAC
 - [ ] Set 07, Challenge 51: Compression Ratio Side-Channel Attacks
 - [ ] Set 07, Challenge 52: Iterated Hash Function Multicollisions
 - [ ] Set 07, Challenge 53: Kelsey and Schneier's Expandable Messages
 - [ ] Set 07, Challenge 54: Kelsey and Kohno's Nostradamus Attack
 - [ ] Set 07, Challenge 55: MD4 Collisions
 - [ ] Set 07, Challenge 56: RC4 Single-Byte Biases
    
### Set 08: Abstract Algebra
 - [ ] Set 08, Challenge 57: Diffie-Hellman Revisited - Small Subgroup
                             Confinement
 - [ ] Set 08, Challenge 58: Pollard's Method for Catching Kangaroos
 - [ ] Set 08, Challenge 59: Elliptic Curve Diffie-Hellman and
                             Invalid-Curve Attacks
 - [ ] Set 08, Challenge 60: Single-Coordinate Ladders and Insecure Twists
 - [ ] Set 08, Challenge 61: Duplicate-Signature Key Selection in ECDSA
                             (and RSA)
 - [ ] Set 08, Challenge 62: Key-Recovery Attacks on ECDSA with Biased
                             Nonces 
 - [ ] Set 08, Challenge 63: Key-Recovery Attacks on GCM with Repeated
                             Nonces
 - [ ] Set 08, Challenge 64: Key-Recovery Attacks on GCM with a Truncated
                             MAC

