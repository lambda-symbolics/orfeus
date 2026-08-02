#!/usr/bin/env python3
"""Convert KAIR's ffdnet_color.pth into Orfeus's embedded weight format.

Reads the legacy (pre-1.6, non-zip) PyTorch serialization of
https://github.com/cszn/KAIR/releases/download/v1.0/ffdnet_color.pth
using only the standard library, and writes a flat little-endian file:

  magic  8 bytes  b"ORFEUSNN"
  u32    format version (1)
  u32    layer count
  per layer:
    u32 output channels, u32 input channels, u32 kernel size
    f32[out*in*k*k] weights in PyTorch OIHW order
    f32[out]        biases

Usage: ffdnet-pth-to-bin.py ffdnet_color.pth ffdnet_color.bin
"""

import pickle
import struct
import sys

MAGIC_NUMBER = 0x1950A86A20F9469CFC6C
EXPECTED_LAYERS = [
    (96, 13), (96, 96), (96, 96), (96, 96), (96, 96), (96, 96),
    (96, 96), (96, 96), (96, 96), (96, 96), (96, 96), (12, 96),
]


class Storage:
    def __init__(self, key, numel):
        self.key = key
        self.numel = numel


class LegacyUnpickler(pickle.Unpickler):
    def find_class(self, module, name):
        if name == "_rebuild_tensor_v2":
            def rebuild(storage, offset, size, stride, *rest):
                return (storage, offset, tuple(size), tuple(stride))
            return rebuild
        if name == "OrderedDict":
            from collections import OrderedDict
            return OrderedDict
        if name.endswith("Storage"):
            if name != "FloatStorage":
                raise ValueError(f"unsupported storage type {name}")
            return name
        raise ValueError(f"unexpected pickle global {module}.{name}")

    def persistent_load(self, pid):
        kind, _cls, key, _location, numel = pid[:5]
        if kind != "storage":
            raise ValueError(f"unexpected persistent id {pid!r}")
        return Storage(key, numel)


def main(source_path, target_path):
    with open(source_path, "rb") as source:
        if LegacyUnpickler(source).load() != MAGIC_NUMBER:
            raise ValueError("not a legacy PyTorch checkpoint")
        LegacyUnpickler(source).load()  # protocol version
        LegacyUnpickler(source).load()  # system info
        state = LegacyUnpickler(source).load()
        keys = LegacyUnpickler(source).load()
        storages = {}
        for key in keys:
            (numel,) = struct.unpack("<q", source.read(8))
            storages[key] = source.read(numel * 4)
        if source.read():
            raise ValueError("trailing bytes after storage data")

    layers = []
    for index in range(len(EXPECTED_LAYERS)):
        weight = state[f"model.{2 * index}.weight"]
        bias = state[f"model.{2 * index}.bias"]
        out_channels, in_channels = EXPECTED_LAYERS[index]
        if weight[2] != (out_channels, in_channels, 3, 3):
            raise ValueError(f"layer {index} weight shape {weight[2]}")
        if bias[2] != (out_channels,):
            raise ValueError(f"layer {index} bias shape {bias[2]}")
        for tensor in (weight, bias):
            storage, offset, size, _stride = tensor
            count = 1
            for dimension in size:
                count *= dimension
            if offset != 0 or storage.numel != count:
                raise ValueError(f"layer {index} tensor is a strided view")
        layers.append((out_channels, in_channels,
                       storages[weight[0].key], storages[bias[0].key]))

    with open(target_path, "wb") as target:
        target.write(b"ORFEUSNN")
        target.write(struct.pack("<II", 1, len(layers)))
        for out_channels, in_channels, weight_bytes, bias_bytes in layers:
            target.write(struct.pack("<III", out_channels, in_channels, 3))
            target.write(weight_bytes)
            target.write(bias_bytes)
    print(f"wrote {target_path}")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2])
